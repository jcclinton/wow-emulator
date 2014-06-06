-module(srp).
-export([normalize/1, getGenerator/0, getPrime/0, generatePrivate/0, getServerPublic/4, computeServerKey/5, interleaveHash/1, b_to_l_endian/2, l_to_b_endian/2, getM1/7, getM2/3]).
-export([getVerifier/3, getDerivedKey/3, getClientPublic/3, computeClientKey/6 ]).

-include("include/binary.hrl").


getGenerator() -> <<7/integer>>.

%% srp version 6
getVersion() -> '6'.

getMultiplier() -> <<3/integer>>.


getSize() -> 256.

% randomly generated 32 byte number
generatePrivate() ->
	Size = getSize(),
	crypto:rand_bytes(Size div 8).


%% 32 byte prime number
%% used in mangos: https://github.com/mangoszero/server/blob/master/src/realmd/AuthSocket.cpp#L190
%% used in arcemu: http://arcemu.org/wiki/Server_Logon_Challenge
getPrime() ->
	<<16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7?QQB>>.



%% v = g^x
getVerifier(Generator, Prime, DerivedKey) ->
	crypto:mod_pow(Generator, DerivedKey, Prime).


%% x = H(salt, H(username, :, password))
getDerivedKey(UBin, PwBin, Salt) ->
	UNormBin = normalize(UBin),
	PwNormBin = normalize(PwBin),
	PassHash = hash([UNormBin, <<$:>>, PwNormBin]),
	X = hash([Salt, PassHash]),
	l_to_b_endian(X, 160).

normalize(BinString) ->
	S1 = binary_to_list(BinString),
	S2 = string:to_upper(S1),
	list_to_binary(S2).

getScrambler(ClientPublic, ServerPublic) ->
	ClientPublicL = b_to_l_endian(ClientPublic, 256),
	ServerPublicL = b_to_l_endian(ServerPublic, 256),
	U = hash([ClientPublicL, ServerPublicL]),
	l_to_b_endian(U, 160).


%% client public key
getClientPublic(Generator, Prime, ClientPrivate) ->
	Version = getVersion(),
																																															{Pub, ClientPrivate} = crypto:generate_key(srp, {user, [Generator, Prime, Version]}, ClientPrivate),
	Pub.

%% server public key
getServerPublic(Generator, Prime, ServerPrivate, Verifier) ->
	Version = getVersion(),
	{Pub, ServerPrivate} = crypto:generate_key(srp, {host, [Verifier, Generator, Prime, Version]}, ServerPrivate),
Pub.


%% client session key
%% doesnt use compute_key because there currently is a bug in otp
%% will be fixed sometime in R17
computeClientKey(ClientPrivate, ServerPublic, ClientPublic, Generator, Prime, DerivedKey) ->
	U = getScrambler(ClientPublic, ServerPublic),
	PrimeI = bin_to_int(Prime),
	Multiplier = getMultiplier(),
	BX = crypto:mod_pow(Generator, DerivedKey, Prime),
	%BX = Verifier,
	BTMPI0 = bin_to_int(ServerPublic) - bin_to_int(Multiplier) * bin_to_int(BX),
	BTMPI = BTMPI0 rem PrimeI,
	Base = if
			BTMPI > 0 -> int_to_bin(BTMPI);
			true -> int_to_bin(BTMPI + PrimeI)
	end,
	Exponent = int_to_bin(bin_to_int(ClientPrivate) + bin_to_int(U) * bin_to_int(DerivedKey)),
	crypto:mod_pow(Base, Exponent, Prime).


%% server session key
computeServerKey(ServerPrivate, ClientPublic, ServerPublic, Prime, Verifier) ->
	U = getScrambler(ClientPublic, ServerPublic),
	Version = getVersion(),
	crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate}, {host, [Verifier, Prime, Version, U]}).

getM1(Prime, Generator, UBin, SaltL, ClientPublic, ServerPublic, Key) ->
	PrimeL = b_to_l_endian(Prime, 256),
	PhashL = hash(PrimeL),
	Phash = l_to_b_endian(PhashL, 160),

	GhashL = hash(Generator),
	Ghash = l_to_b_endian(GhashL, 160),

	P1 = crypto:exor(Phash, Ghash),
	P1L = b_to_l_endian(P1, 160),

	UNorm = normalize(UBin),
	NameHash = hash(UNorm),

	ServerPublicL = b_to_l_endian(ServerPublic, 256),
	ClientPublicL = b_to_l_endian(ClientPublic, 256),
	KeyL = b_to_l_endian(Key, 320),
	L = [P1L, NameHash, SaltL, ClientPublicL, ServerPublicL, KeyL],
	Ml = hash(L),
	l_to_b_endian(Ml, 160).


getM2(ClientPublic, M1, Key) ->
	ClientPublicL = b_to_l_endian(ClientPublic, 256),
	KeyL = b_to_l_endian(Key, 320),
	M1L = b_to_l_endian(M1, 160),
	M2 = hash([ClientPublicL, M1L, KeyL]),
	l_to_b_endian(M2, 160).





interleaveHash(Input) ->
	InputL = b_to_l_endian(Input, 256),
	%% todo remove all zero bytes from beginning of input
	{Even, Odd} = getBytes(InputL, {[], []}, 0),
	EvenHash = hash(Even),
	OddHash = hash(Odd),
	Out = combineHashes(EvenHash, OddHash, []),
	%io:format("input: ~p~nE: ~p~nF: ~p~nG: ~p~nH: ~p~nout: ~p~n", [Input, E, F, G, H, Out]),
	OutBin = iolist_to_binary(Out),
	l_to_b_endian(OutBin, 320).

% returns tuple of even bytes and odd bytes
getBytes(<<>>, Data, _N) -> Data;
getBytes(<<Byte?B, Rest/binary>>, {Even, Odd}, N) ->
	IsEven = N rem 2 == 0,
	NewData = if IsEven ->
			NewEven = Even ++ [Byte],
			{NewEven, Odd};
		true ->
			NewOdd = Odd ++ [Byte],
			{Even, NewOdd}
		end,
	getBytes(Rest, NewData, N+1).

combineHashes(<<>>, <<>>, Data) -> lists:reverse(Data);
combineHashes(<<Even0?B, EvenRest/binary>>, <<Odd0?B, OddRest/binary>>, Data) ->
	NewData = [Odd0 | [Even0 | Data]],
	combineHashes(EvenRest, OddRest, NewData).






%% utility functions
hash(L) -> crypto:hash(sha, L).

l_to_b_endian(Msg, 256) ->
	<<Num?QQ>> = Msg,
	<<Num?QQB>>;
l_to_b_endian(Msg, 160) ->
	<<Num?SH>> = Msg,
	<<Num?SHB>>;
l_to_b_endian(Msg, 320) ->
	<<Num?SL>> = Msg,
	<<Num?SLB>>.

b_to_l_endian(Msg, 256) ->
	<<Num?QQB>> = Msg,
	<<Num?QQ>>;
b_to_l_endian(Msg, 160) ->
	<<Num?SHB>> = Msg,
	<<Num?SH>>;
b_to_l_endian(Msg, 320) ->
	<<Num?SLB>> = Msg,
	<<Num?SL>>.


int_to_bin(Int) ->
	Len0 = length(erlang:integer_to_list(Int, 16)),
	Len1 = Len0 + (Len0 rem 2),
	Bits = Len1 * 4,
	<<Int:Bits>>.

bin_to_int(Bin) ->
	Bits = byte_size(Bin) * 8,
	<<Val:Bits/unsigned-little-integer>> = Bin,
	Val.
