-module(world_crypto).

-export([encrypt/2, decrypt/2, encryption_key/1]).
-export([send_packet/6, receive_packet/4]).

-include("binary.hrl").
-include("include/network_defines.hrl").

-define(K, 40).

%% @spec encrypt(binary(), binary()) -> binary().
encrypt(Header, Key) ->
    encrypt(Header, Key, <<>>).

%% @spec encrypt(binary(), binary(), binary()) -> binary().
encrypt(<<>>, Key, Result) -> {Result, Key};
encrypt(<<OldByte?B, Header/binary>>, {SI, SJ, K}, Result) ->
    NewByte = ((lists:nth(SI+1, K) bxor OldByte) + SJ) band 255,
    NewSI   = (SI+1) rem ?K,
    encrypt(Header, {NewSI, NewByte, K}, <<Result/binary, NewByte:8>>).

%% @spec decrypt(binary(), binary()) -> binary().
decrypt(Header, Key) ->
    decrypt(Header, Key, <<>>).

%% @spec decrypt(binary(), binary(), binary()) -> binary().
decrypt(<<>>, Key, Result) -> {Result, Key};
decrypt(<<OldByte?B, Header/binary>>, {RI, RJ, K}, Result) ->
    NewByte = (lists:nth(RI+1, K) bxor (OldByte - RJ)) band 255,
    NewRI   = (RI + 1) rem ?K,
    decrypt(Header, {NewRI, OldByte, K}, <<Result/binary, NewByte:8>>).

%% @spec encrypt(string()) -> list().
encryption_key(A) ->
	K = char_data:get_session_key(A),
	binary_to_list(K).


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


send_packet(OpAtom, Payload, HdrLen, KeyState, Socket, ShouldEncrypt) ->
	Opcode = opcodes:getNumByAtom(OpAtom),
	OpBin = if HdrLen == ?SEND_HDR_LEN -> <<Opcode?W>>;
		HdrLen == ?RCV_HDR_LEN -> <<Opcode?L>>
	end,
	Length = size(OpBin) + size(Payload),
	Header = <<Length?WO, OpBin/binary>>,
	{HeaderOut, NewKeyState} = if ShouldEncrypt -> encrypt(Header, KeyState);
		not ShouldEncrypt -> {Header, KeyState}
	end,
	Packet = <<HeaderOut/binary, Payload/binary>>,
	case gen_tcp:send(Socket, Packet) of
		ok -> NewKeyState;
		{eror, Error} -> throw(Error)
	end.
