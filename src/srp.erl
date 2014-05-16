-module(srp).
-export([test/0]).
-compile([export_all]).


getUsername() -> <<"alice">>.

getPassword() -> <<"password123">>.

getSalt() -> <<"mystrongsalt">>.

getGenerator() -> <<7/integer>>.

%% srp version 6
getVersion() -> '6'.

getMultiplier() -> <<3/integer>>.

getSize() -> 256.

% randomly generated 32 byte number
getClientPrivate() ->
	generatePrivate().

% randomly generated 32 byte number
getServerPrivate() ->
	generatePrivate().

generatePrivate() ->
	Size = getSize(),
	crypto:rand_bytes(Size div 8).


%% 32 byte prime number
%% used in mangos: https://github.com/mangoszero/server/blob/master/src/realmd/AuthSocket.cpp#L190
%% used in arcemu: http://arcemu.org/wiki/Server_Logon_Challenge
getPrime() ->
	Size = getSize(),
	<<16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7:Size>>.



%% v = g^x
getVerifier() ->
	Generator = getGenerator(),
	Prime = getPrime(),
	DerivedKey = getDerivedKey(),
	crypto:mod_pow(Generator, DerivedKey, Prime).


%% x = H(salt, H(username, :, password))
getDerivedKey() ->
	Username = getUsername(),
	Password = getPassword(),
	Salt = getSalt(),
	crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]).

getScrambler(ClientPublic, ServerPublic) ->
	crypto:hash(sha, [ClientPublic, ServerPublic]).


%% client public key
getClientPublic(ClientPrivate) ->
	Generator = getGenerator(),
	Prime = getPrime(),
	Version = getVersion(),
																																															{Pub, ClientPrivate} = crypto:generate_key(srp, {user, [Generator, Prime, Version]}, ClientPrivate),
	Pub.

%% server public key
getServerPublic(ServerPrivate) ->
	Generator = getGenerator(),
	Prime = getPrime(),
	Version = getVersion(),
	Verifier = getVerifier(),
	{Pub, ServerPrivate} = crypto:generate_key(srp, {host, [Verifier, Generator, Prime, Version]}, ServerPrivate),
Pub.


%% client session key
%% doesnt use compute_key because there currently is a bug in otp
computeClientKey(ClientPrivate, ServerPublic) ->
	ClientPublic = getClientPublic(ClientPrivate),
	U = getScrambler(ClientPublic, ServerPublic),
	Generator = getGenerator(),
	Prime = getPrime(),
	DerivedKey = getDerivedKey(),
	PrimeI = bin_to_int(Prime),
	Multiplier = getMultiplier(),
	BX = crypto:mod_pow(Generator, DerivedKey, Prime),
	BTMPI0 = bin_to_int(ServerPublic) - bin_to_int(Multiplier) * bin_to_int(BX),
	BTMPI = BTMPI0 rem PrimeI,
	Base = if
			BTMPI > 0 -> int_to_bin(BTMPI);
			true -> int_to_bin(BTMPI + PrimeI)
	end,
	Exponent = int_to_bin(bin_to_int(ClientPrivate) + bin_to_int(U) * bin_to_int(DerivedKey)),
	crypto:mod_pow(Base, Exponent, Prime).


%% server session key
computeServerKey(ServerPrivate, ClientPublic) ->
	ServerPublic = getServerPublic(ServerPrivate),
	U = getScrambler(ClientPublic, ServerPublic),
	Prime = getPrime(),
	Version = getVersion(),
	Verifier = getVerifier(),
	crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate}, {host, [Verifier, Prime, Version, U]}).



test() ->
	%% these session keys should match
	ClientPrivate = generatePrivate(),
	ServerPrivate = generatePrivate(),
	ServerPublic = getServerPublic(ServerPrivate),
	ClientPublic = getClientPublic(ClientPrivate),
	ClientKey = computeClientKey(ClientPrivate, ServerPublic),
	ServerKey = computeServerKey(ServerPrivate, ClientPublic),
	io:format("client skey: ~p~n", [ClientKey]),
	io:format("server skey: ~p~n", [ServerKey]),
	ClientKey == ServerKey.


%% utility functions
int_to_bin(Int) ->
	Len0 = length(erlang:integer_to_list(Int, 16)),
	Len1 = Len0 + (Len0 rem 2),
	Bits = Len1 * 4,
	<<Int:Bits>>.

bin_to_int(Bin) ->
	Bits = byte_size(Bin) * 8,
	<<Val:Bits>> = Bin,
	Val.
