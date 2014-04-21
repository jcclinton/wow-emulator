-module(logon_lib).
-export([getUsername/0, getPassword/0, getClientPrivate/0, getServerPrivate/0, getSalt/0, getGenerator/0, getPrime/0]).
-export([getClientPublic/0, getServerPublic/0, computeClientKey/1, computeServerKey/1, test/0]).
-export([getM/3, hash/1, getVersion/0]).

-compile([export_all]).

-include("include/binary.hrl").


getUsername() ->
	<<"androsynth">>.

getPassword() ->
	<<"Poners2431!">>.


getSalt() -> <<16#b3d47dc40109ba25459096abdd0bfbbc3266d5b2dcf52eb586d2d2e612afdd84?QQ>>.



getPrime1() -> <<16#EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE48E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B297BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9AFD5138FE8376435B9FC61D2FC0EB06E3:1024>>.
getPrime2() -> <<7?QQ>>.

getPrime() ->
	Num = 16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7,
	<<Num:256/unsigned-little-integer>>.

getGenerator() -> <<7>>.

getVersion() -> '6'.


getScrambler(ServerPublic, ClientPublic) ->
	hash([ClientPublic, ServerPublic]).


getVerifier() ->
	G = getGenerator(),
	P = getPrime(),
	DerivedKey = getDerivedKey(),
	crypto:mod_pow(G, DerivedKey, P).


getClientPrivate() -> <<16#60975527035CF2AD1989806F0407210BC81EDC04E2762A56AFD529DDDA2D4393?QQ>>.


getServerPrivate() -> <<16#E487CB59D31AC550471E81F00F6928E01DDA08E974A004F49E61F5D105284D20?QQ>>.

getDerivedKey() ->
	U = getUsername(),
	P = getPassword(),
	Salt = getSalt(),
	hash([Salt, hash([U, <<$:>>, P])]).

hash(L) ->
	crypto:hash(sha, L).



%% public
getClientPublic() ->
	Priv = getClientPrivate(),
	G = getGenerator(),
	P = getPrime(),
	Version = getVersion(),
	{Pub, _} = crypto:generate_key(srp, {user, [G, P, Version]}, Priv),
	Pub.

getServerPublic() ->
	Priv = getServerPrivate(),
	G = getGenerator(),
	P = getPrime(),
	Version = getVersion(),
	V = getVerifier(),
	{Pub, _} = crypto:generate_key(srp, {host, [V, G, P, Version]}, Priv),
	Pub.


computeClientKey(ServerPublic) ->
	ClientPrivate = getClientPrivate(),
	ClientPublic = getClientPublic(),
	G = getGenerator(),
	P = getPrime(),
	Version = getVersion(),
	U = getScrambler(ServerPublic, ClientPublic),
	DerivedKey = getDerivedKey(),
	crypto:compute_key(srp, ServerPublic, {ClientPublic, ClientPrivate}, {user, [DerivedKey, P, G, Version, U]}).

computeServerKey(ClientPublic) ->
	ServerPrivate = getServerPrivate(),
	ServerPublic = getServerPublic(),
	P = getPrime(),
	Version = getVersion(),
	U = getScrambler(ServerPublic, ClientPublic),
	V = getVerifier(),
	crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate}, {host, [V, P, Version, U]}).

getM(Apub, Bpub, S) ->
	P = getPrime(),
	G = getGenerator(),
	I = getUsername(),
	P1 = crypto:exor(hash(P), hash(G)),
	Salt = getSalt(),
	K = hash([S]),
	M = hash([P1, hash(I), Salt, Apub, Bpub, K]),
	M.

getM2(Apub, M1, Skey) ->
	hash([Apub, M1, Skey]).

test() ->
	ClientPublic = getClientPublic(),
	ServerPublic = getServerPublic(),
	ClientKey = computeClientKey(ServerPublic),
	ServerKey = computeServerKey(ClientPublic),
	Prime = size(getPrime()),
	io:format("prime is ~p bytes ~n", [Prime]),
	hash(ClientKey) == hash(ServerKey).

