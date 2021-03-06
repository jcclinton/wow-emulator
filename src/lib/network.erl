%%   This is a World of Warcraft emulator written in erlang, supporting
%%   client 1.12.x
%%
%%   Copyright (C) 2014  Jamie Clinton <jamieclinton.com>
%%
%%   This program is free software; you can redistribute it and/or modify
%%   it under the terms of the GNU General Public License as published by
%%   the Free Software Foundation; either version 2 of the License, or
%%   (at your option) any later version.
%%
%%   This program is distributed in the hope that it will be useful,
%%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%   GNU General Public License for more details.
%%
%%   You should have received a copy of the GNU General Public License along
%%   with this program; if not, write to the Free Software Foundation, Inc.,
%%   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%
%%   World of Warcraft, and all World of Warcraft or Warcraft art, images,
%%   and lore ande copyrighted by Blizzard Entertainment, Inc.

-module(network).

-export([send_packet/6, send_queue/5]).
-export([receive_packet/4]).

-include("include/binary.hrl").
-include("include/network_defines.hrl").
-include("include/data_types.hrl").



% receive and decrypt a packet
-spec receive_packet(non_neg_integer(), maybe_key_state(), term(), boolean()) -> {non_neg_integer(), binary(), key_state()}.
receive_packet(HdrLen, KeyState, Socket, ShouldDecrypt) ->
	{ok, HeaderIn} = case gen_tcp:recv(Socket, HdrLen) of
		{error, ErrorIn} -> throw(ErrorIn);
		Result -> Result
	end,

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
	{Opcode, Payload, NewKeyState}.


% take queue of packets and combine them into a single binary
-spec send_queue(queue:queue(term()), non_neg_integer(), key_state(), term(), boolean()) -> key_state().
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
-spec build_packet(non_neg_integer(), binary(), non_neg_integer(), key_state(), boolean()) -> {binary(), key_state()}.
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
-spec send_packet(non_neg_integer(), binary(), non_neg_integer(), maybe_key_state(), term(), boolean()) -> key_state().
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
		{error, Error} -> throw(Error)
	end.
