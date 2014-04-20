-module(logon_lib).
-export([getUsername/0, getPassword/0, getClientPrivate/0, getServerPrivate/0, getSalt/0, getGenerator/0, getPrime/0]).
-export([getClientPublic/0, getServerPublic/0, computeClientKey/1, computeServerKey/1, test/0]).
-export([getM/3, hash/1]).



getUsername() ->
	<<"jcclinton24">>.

getPassword() ->
	<<"abc123">>.


getSalt() ->
	hexstr2bin("BEB25379D1A8581EB5A727673A2441EE").



getPrime() ->
	hexstr2bin("EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C"
							"9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4"
							"8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29"
							"7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A"
							"FD5138FE8376435B9FC61D2FC0EB06E3").

getGenerator() -> <<2>>.

getVersion() -> '6a'.


getScrambler(ServerPublic, ClientPublic) ->
	hash([ClientPublic, ServerPublic]).


getVerifier() ->
	G = getGenerator(),
	P = getPrime(),
	DerivedKey = getDerivedKey(),
	crypto:mod_pow(G, DerivedKey, P).


getClientPrivate() -> hexstr2bin("60975527035CF2AD1989806F0407210BC81EDC04E2762A56AFD529DD"
																"DA2D4393").


getServerPrivate() ->hexstr2bin("E487CB59D31AC550471E81F00F6928E01DDA08E974A004F49E61F5D1"
																"05284D20").

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

