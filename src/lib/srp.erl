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

-module(srp).
-export([normalize/1, getGenerator/0, getPrime/0, generatePrivate/0, computeServerKey/5, interleaveHash/1, b_to_l_endian/2, l_to_b_endian/2, getM1/7, getM2/3]).
-export([getVerifier/3, getDerivedKey/3, getClientPublicPrivate/2, computeClientKey/6 ]).
-export([getServerPublicPrivate/3]).

-export([test/0]).

-include("include/binary.hrl").

-define(sha, 160).
-define(bits_size, 256).


test() ->
	Prime = getPrime(),
	Generator = getGenerator(),

	UBin = <<"alice">>,
	PwBin = <<"password123">>,


	Salt = generatePrivate(),
	DerivedKey = getDerivedKey(UBin, PwBin, Salt),
	Verifier = getVerifier(Generator, Prime, DerivedKey),

	{ClientPublic, ClientPrivate} = getClientPublicPrivate(Generator, Prime),
	{ServerPublic, ServerPrivate} = getServerPublicPrivate(Generator, Prime, Verifier),

	ServerKey = computeServerKey(ServerPrivate, ClientPublic, ServerPublic, Prime, Verifier),
	ClientKey = computeClientKey(ClientPrivate, ServerPublic, ClientPublic, Generator, Prime, DerivedKey),

	io:format("client key: ~p~nserver key: ~p~n", [ClientKey, ServerKey]),
	ServerKey == ClientKey.


getGenerator() -> <<7?B>>.

%% srp version 6
getVersion() -> '6'.

%getMultiplier() -> <<3?B>>.


getSize() ->
	?bits_size.
	%bit_size(getPrime()).
getBytes() -> getSize() div 8.

% randomly generated 32 byte number
generatePrivate() ->
	Bytes = getBytes(),
	crypto:strong_rand_bytes(Bytes).


%% 32 byte prime number
%% used in mangos: https://github.com/mangoszero/server/blob/master/src/realmd/AuthSocket.cpp#L190
%% used in arcemu: http://arcemu.org/wiki/Server_Logon_Challenge
getPrime() ->
	Size = getSize(),
	get_prime(Size).

	get_prime(256) ->
		<<16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7?QQB>>;
	get_prime(1024) ->
	<<16#EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE48E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B297BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9AFD5138FE8376435B9FC61D2FC0EB06E3?K>>.




%% v = g^x
getVerifier(Generator, Prime, DerivedKey) ->
	crypto:mod_pow(Generator, DerivedKey, Prime).


%% x = H(salt, H(username, :, password))
getDerivedKey(UBin, PwBin, Salt) ->
	UNormBin = normalize(UBin),
	PwNormBin = normalize(PwBin),
	PassHash = hash([UNormBin, <<$:>>, PwNormBin]),
	X = hash([Salt, PassHash]),
	l_to_b_endian(X, ?sha).

normalize(BinString) ->
	S1 = binary_to_list(BinString),
	S2 = string:to_upper(S1),
	list_to_binary(S2).

getScrambler(ClientPublic, ServerPublic) ->
	Size = getSize(),
	ClientPublicL = b_to_l_endian(ClientPublic, Size),
	ServerPublicL = b_to_l_endian(ServerPublic, Size),
	U = hash([ClientPublicL, ServerPublicL]),
	l_to_b_endian(U, ?sha).


%% client public key
getClientPublicPrivate(Generator, Prime) ->
	Version = getVersion(),
	{Pub, Priv} = crypto:generate_key(srp, {user, [Generator, Prime, Version]}),
	Size = byte_size(Pub),
	Bytes = getBytes(),
	%sometimes server pub comes up less than required size and needs to be regenerated
	if Bytes /= Size ->
			getClientPublicPrivate(Generator, Prime);
		Bytes == Size -> {Pub, Priv}
	end.

%generates a public private key pair
getServerPublicPrivate(Generator, Prime, Verifier) ->
	Version = getVersion(),
	{Pub, ServerPrivate} = crypto:generate_key(srp, {host, [Verifier, Generator, Prime, Version]}),
	Size = byte_size(Pub),
	Bytes = getBytes(),
	%sometimes server pub comes up less than required size and needs to be regenerated
	if Bytes /= Size ->
			getServerPublicPrivate(Generator, Prime, Verifier);
		Bytes == Size -> {Pub, ServerPrivate}
	end.


%% client session key
computeClientKey(ClientPrivate, ServerPublic, ClientPublic, Generator, Prime, DerivedKey) ->
	U = getScrambler(ClientPublic, ServerPublic),
	Version = getVersion(),
	crypto:compute_key(srp, ServerPublic, {ClientPublic, ClientPrivate}, {user, [DerivedKey, Prime, Generator, Version, U]}).


%% server session key
computeServerKey(ServerPrivate, ClientPublic, ServerPublic, Prime, Verifier) ->
	U = getScrambler(ClientPublic, ServerPublic),
	Version = getVersion(),
	Key = crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate}, {host, [Verifier, Prime, Version, U]}),
	Size = byte_size(Key),
	Bytes = getBytes(),
	%sometimes server key comes up less than required size and needs to be regenerated
	if Bytes /= Size ->
		computeServerKey(ServerPrivate, ClientPublic, ServerPublic, Prime, Verifier);
		Bytes == Size -> Key
	end.

getM1(Prime, Generator, UBin, SaltL, ClientPublic, ServerPublic, Key) ->
	Size = getSize(),
	PrimeL = b_to_l_endian(Prime, Size),
	PhashL = hash(PrimeL),
	Phash = l_to_b_endian(PhashL, ?sha),

	GhashL = hash(Generator),
	Ghash = l_to_b_endian(GhashL, ?sha),

	P1 = crypto:exor(Phash, Ghash),
	P1L = b_to_l_endian(P1, ?sha),

	UNorm = normalize(UBin),
	NameHash = hash(UNorm),

	ServerPublicL = b_to_l_endian(ServerPublic, Size),
	ClientPublicL = b_to_l_endian(ClientPublic, Size),
	KeyL = b_to_l_endian(Key, 2 * ?sha),
	L = [P1L, NameHash, SaltL, ClientPublicL, ServerPublicL, KeyL],
	Ml = hash(L),
	l_to_b_endian(Ml, ?sha).


getM2(ClientPublic, M1, Key) ->
	Size = getSize(),
	ClientPublicL = b_to_l_endian(ClientPublic, Size),
	KeyL = b_to_l_endian(Key, 2 * ?sha),
	M1L = b_to_l_endian(M1, ?sha),
	M2 = hash([ClientPublicL, M1L, KeyL]),
	l_to_b_endian(M2, ?sha).





interleaveHash(Input) ->
	Size = getSize(),
	InputL = b_to_l_endian(Input, Size),
	%% todo remove all zero bytes from beginning of input
	{Even, Odd} = separateBytes(InputL, {[], []}, 0),
	EvenHash = hash(Even),
	OddHash = hash(Odd),
	Out = combineHashes(EvenHash, OddHash, []),
	%io:format("input: ~p~nE: ~p~nF: ~p~nG: ~p~nH: ~p~nout: ~p~n", [Input, E, F, G, H, Out]),
	OutBin = iolist_to_binary(Out),
	l_to_b_endian(OutBin, 2 * ?sha).

% returns tuple of even bytes and odd bytes
separateBytes(<<>>, Data, _N) -> Data;
separateBytes(<<Byte?B, Rest/binary>>, {Even, Odd}, N) ->
	IsEven = N rem 2 == 0,
	NewData = if IsEven ->
			NewEven = Even ++ [Byte],
			{NewEven, Odd};
		true ->
			NewOdd = Odd ++ [Byte],
			{Even, NewOdd}
		end,
	separateBytes(Rest, NewData, N+1).

combineHashes(<<>>, <<>>, Data) -> lists:reverse(Data);
combineHashes(<<Even0?B, EvenRest/binary>>, <<Odd0?B, OddRest/binary>>, Data) ->
	NewData = [Odd0 | [Even0 | Data]],
	combineHashes(EvenRest, OddRest, NewData).






%% utility functions
hash(L) -> crypto:hash(sha, L).

l_to_b_endian(Msg, 1024) ->
	<<Num?K>> = Msg,
	<<Num?KB>>;
l_to_b_endian(Msg, 256) ->
	<<Num?QQ>> = Msg,
	<<Num?QQB>>;
l_to_b_endian(Msg, 160) ->
	<<Num?SH>> = Msg,
	<<Num?SHB>>;
l_to_b_endian(Msg, 320) ->
	<<Num?SD>> = Msg,
	<<Num?SDB>>.

b_to_l_endian(Msg, 1024) ->
	<<Num?KB>> = Msg,
	<<Num?K>>;
b_to_l_endian(Msg, 256) ->
	<<Num?QQB>> = Msg,
	<<Num?QQ>>;
b_to_l_endian(Msg, 160) ->
	<<Num?SHB>> = Msg,
	<<Num?SH>>;
b_to_l_endian(Msg, 320) ->
	<<Num?SDB>> = Msg,
	<<Num?SD>>.
