-module(world_crypto).

-export([encrypt/2, decrypt/2, encryption_key/1]).
-export([send_packet/6]).

-include("binary.hrl").

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


send_packet(OpAtom, Payload, HdrLen, KeyState, Socket, ShouldEncrypt) ->
	Opcode = opcodes:getNumByAtom(OpAtom),
	OpBin = if HdrLen == 4 -> <<Opcode?W>>;
		HdrLen == 6 -> <<Opcode?L>>
	end,
	Length = size(OpBin) + size(Payload),
	Header = <<Length?WO, OpBin/binary>>,
	{HeaderOut, NewKeyState} = if ShouldEncrypt -> encrypt(Header, KeyState);
		not ShouldEncrypt -> {Header, KeyState}
	end,
	Packet = <<HeaderOut/binary, Payload/binary>>,
	gen_tcp:send(Socket, Packet),
	NewKeyState.
