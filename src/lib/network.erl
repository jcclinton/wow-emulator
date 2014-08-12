-module(network).

-export([send_packet/6, send_queue/5]).
-export([receive_packet/4]).

-include("binary.hrl").
-include("include/network_defines.hrl").


% receive and decrypt a packet
receive_packet(HdrLen, KeyState, Socket, ShouldDecrypt) ->
	case gen_tcp:recv(Socket, HdrLen) of
		{ok, HeaderIn} ->
			{Header, NewKeyState} = if ShouldDecrypt ->
					world_crypto:decrypt(HeaderIn, KeyState);
				not ShouldDecrypt ->
					{HeaderIn, KeyState}
			end,

			{LengthRaw, Opcode} = if HdrLen == ?RCV_HDR_LEN ->
					<<LenRaw?WO, Op?L>> = Header,
					{LenRaw, Op};
				HdrLen == ?SEND_HDR_LEN ->
					<<LenRaw?WO, Op?W>> = Header,
					{LenRaw, Op}
			end,

			% size of length is always 2
			OpSize = HdrLen - 2,
			Length = LengthRaw - OpSize,
			%io:format("player rcv: received opcode ~p with length ~p on account ~p~n", [Opcode, Length, AccountId]),
			Payload = if Length > 0 ->
					case gen_tcp:recv(Socket, Length) of
						{ok, Data} -> Data;
						{error, Error} -> throw(Error)
					end;
				Length == 0 -> <<>>;
				Length < 0 -> throw(bad_header_length)
			end,
			{Opcode, Payload, NewKeyState};
		{error, Error} -> throw(Error)
	end.


% take queue of packets and combine them into a single binary
send_queue(Queue, HdrLen, KeyState, Socket, ShouldEncrypt) ->
	List = queue:to_list(Queue),
	{PacketOut, KeyStateOut} = lists:foldl(fun({Opcode, Payload}, {BinAcc, KeyStateAcc}) ->
		{Bin, NewKeyState} = build_packet(Opcode, Payload, HdrLen, KeyStateAcc, ShouldEncrypt),
		NewBinAcc = <<BinAcc/binary, Bin/binary>>,
		{NewBinAcc, NewKeyState}
	end, {<<>>, KeyState}, List),
	gen_tcp:send(Socket, PacketOut),
	KeyStateOut.

% build binary packet from the data of a single message
build_packet(Opcode, Payload, HdrLen, KeyState, ShouldEncrypt) ->
	OpBin = if HdrLen == ?SEND_HDR_LEN -> <<Opcode?W>>;
		HdrLen == ?RCV_HDR_LEN -> <<Opcode?L>>
	end,
	Length = size(OpBin) + size(Payload),
	Header = <<Length?WO, OpBin/binary>>,
	{HeaderOut, NewKeyState} = if ShouldEncrypt -> world_crypto:encrypt(Header, KeyState);
		not ShouldEncrypt -> {Header, KeyState}
	end,
	{<<HeaderOut/binary, Payload/binary>>, NewKeyState}.



% take single packet data and send it
send_packet(Opcode, Payload, HdrLen, KeyState, Socket, ShouldEncrypt) ->
	OpBin = if HdrLen == ?SEND_HDR_LEN -> <<Opcode?W>>;
		HdrLen == ?RCV_HDR_LEN -> <<Opcode?L>>
	end,
	Length = size(OpBin) + size(Payload),
	Header = <<Length?WO, OpBin/binary>>,
	{HeaderOut, NewKeyState} = if ShouldEncrypt -> world_crypto:encrypt(Header, KeyState);
		not ShouldEncrypt -> {Header, KeyState}
	end,
	Packet = <<HeaderOut/binary, Payload/binary>>,
	case gen_tcp:send(Socket, Packet) of
		ok -> NewKeyState;
		{eror, Error} -> throw(Error)
	end.
