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


getSalt() -> hexstr2bin("b3d47dc40109ba25459096abdd0bfbbc3266d5b2dcf52eb586d2d2e612afdd84").
%getSalt() -> hexstr2bin("BEB25379D1A8581EB5A727673A2441EEBEB25379D1A8581EB5A727673A2441EE").



%getPrime() -> <<16#89, 16#4B, 16#64, 16#5E, 16#89, 16#E1, 16#53, 16#5B, 16#BD, 16#AD, 16#5B, 16#8B, 16#29, 16#06, 16#50, 16#53, 16#08, 16#01, 16#B1, 16#8E, 16#BF, 16#BF, 16#5E, 16#8F, 16#AB, 16#3C, 16#82, 16#87, 16#2A, 16#3E, 16#9B, 16#B7>>.
%getPrime() -> getPrime2().
%getPrime() -> <<183,155,62,42,135,130,60,171,143,94,191,191,142,177,1,8,83,80,6,41,139,91,173,189,91,83,225,137,94,100,75,137>>.
%getPrime() -> <<16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7?QQ>>.
%getPrime() -> <<16#B79B3E2A87823CAB8F5EBFBF8EB10108535006298B5BADBD5B53E1895E644B89?QQ>>.
getPrime9() ->
	Num = 16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7,
	<<Num?QQ>>.
%getPrime() -> <<7?QQ>>.
getPrime() -> <<62100066509156017342069496140902949863249758336000796928566441170293728648119/unsigned-little-integer>>.
getPrime3() -> <<
16#89?B,
16#4b?B,
16#64?B,
16#5e?B,
16#89?B,
16#53?B,
16#e1?B,
16#5b?B,
16#bd?B,
16#ad?B,
16#5b?B,
16#8b?B,
16#29?B,
16#06?B,
16#50?B,
16#53?B,
16#08?B,
16#01?B,
16#8e?B,
16#b1?B,
16#bf?B,
16#bf?B,
16#5e?B,
16#ab?B,
16#8f?B,
16#3c?B,
16#82?B,
16#87?B,
16#2a?B,
16#3e?B,
16#9b?B,
16#b7?B
>>.
getPrime5() -> <<
16#b7?B,
16#9b?B,
16#3e?B,
16#2a?B,
16#87?B,
16#82?B,
16#3c?B,
16#ab?B,
16#8f?B,
16#5e?B,
16#bf?B,
16#bf?B,
16#8e?B,
16#b1?B,
16#01?B,
16#08?B,
16#53?B,
16#50?B,
16#06?B,
16#29?B,
16#8b?B,
16#5b?B,
16#ad?B,
16#bd?B,
16#5b?B,
16#53?B,
16#e1?B,
16#89?B,
16#5e?B,
16#64?B,
16#4b?B,
16#89?B
>>.

getPrime8() -> <<"B79B3E2A87823CAB8F5EBFBF8EB10108535006298B5BADBD5B53E1895E644B89"?QQ>>.
%getPrime() -> <<"B79B3E2A87823CAB8F5EBFBF8EB1010">>.

%getPrime() -> hexstr2bin("894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7").
getPrime2() ->
	hexstr2bin("EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C"
							"9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4"
							"8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29"
							"7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A"
							"FD5138FE8376435B9FC61D2FC0EB06E3").

getGenerator() -> <<7>>.

getVersion() -> '6'.


getScrambler(ServerPublic, ClientPublic) ->
	hash([ClientPublic, ServerPublic]).


getVerifier() ->
	G = getGenerator(),
	P = getPrime(),
	DerivedKey = getDerivedKey(),
	crypto:mod_pow(G, DerivedKey, P).


getClientPrivate() -> hexstr2bin("60975527035CF2AD1989806F0407210BC81EDC04E2762A56AFD529DDDA2D4393").


getServerPrivate() ->hexstr2bin("E487CB59D31AC550471E81F00F6928E01DDA08E974A004F49E61F5D105284D20").

getDerivedKey() ->
	U = getUsername(),
	P = getPassword(),
	Salt = getSalt(),
	%hash([Salt, P]).
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

test() ->
	ClientPublic = getClientPublic(),
	ServerPublic = getServerPublic(),
	ClientKey = computeClientKey(ServerPublic),
	ServerKey = computeServerKey(ClientPublic),
	Prime = size(getPrime()),
	io:format("prime is ~p bytes ~n", [Prime]),
	hash(ClientKey) == hash(ServerKey).




%% private helper functions

hexstr2bin(S) ->
	list_to_binary(hexstr2list(S)).

hexstr2list([X,Y|T]) ->
	[mkint(X)*16 + mkint(Y) | hexstr2list(T)];
hexstr2list([]) ->
	[].

mkint(C) when $0 =< C, C =< $9 ->
	C - $0;
mkint(C) when $A =< C, C =< $F ->
	C - $A + 10;
mkint(C) when $a =< C, C =< $f ->
	C - $a + 10.

