-module(logon_lib).
-export([getUsername/0, getPassword/0, getClientPrivate/0, getServerPrivate/0, getSalt/0, getGenerator/0, getPrime/0]).
-export([getClientPublic/0, getServerPublic/0, computeClientKey/0, computeServerKey/0]).
-export([getM/3, hash/1, getVersion/0]).
-export([test/0]).

-compile([export_all]).

-include("include/binary.hrl").



%% number of bits we are using
getSize() -> 256.





getUsername() ->
	<<"alice">>.

getPassword() ->
	<<"password123">>.


%getSalt() -> <<16#b3d47dc40109ba25459096abdd0bfbbc3266d5b2dcf52eb586d2d2e612afdd84:256>>.
getSalt() -> <<"mystrongsalt">>.

getMult() ->
	Version = getVersion(),
	case Version of
				'6' ->
						P = getPrime(),
						crypto:mod_pow(<<3>>, <<1>>, P);
				'6a' ->
					P = getPrime(),
					G = getGenerator(),
					hash([P, G])
			end.

getGenerator() ->
	P = getPrime(),
	crypto:mod_pow(<<2>>, <<1>>, P).

getVersion() -> '6'.






getPrime() ->
	Size = getSize(),
	getPrime(Size).

getPrime(8 = Size) ->
	%true
	Num = 16#07,
	<<Num:Size>>;
getPrime(16 = Size) ->
	%false
	Num = 16#aceb,
	<<Num:Size>>;
getPrime(32 = Size) ->
	%false
	Num = 16#c6349fa3,
	<<Num:Size>>;
getPrime(64 = Size) ->
	%false
	Num = 16#89bf07e2dcba6b23,
	<<Num:Size>>;
getPrime(128 = Size) ->
	%false
	Num = 16#bd35b98c15a0fa8c5c243f752f6e1eeb,
	<<Num:Size>>;
getPrime(256 = Size) ->
	%false
	Num = 16#f81217149d53d50d4a8adb0bbf57a5631d5d16218971c9b419f27f3828e98ac3,
	<<Num:Size>>;
getPrime(512 = Size) ->
	%true
	Num = 16#008253ea781be9e6423746924c4b16c6ea9bfdf51abb67bebf1c1259bbba1e8a8b5141eb5fc1d573e5915c065b81b23d0f14ff634ddb92fd73a110a284c6c74aeb,
	<<Num:Size>>;
getPrime(1024 = Size) ->
	%true
	Num = 16#008e3a4c67a5ac5243a69f856dd3e12b2694c0f5f9a1956f5b3b69b83574cc47713146a981e496280290fe3dd7f231bf2328bedbd8b90d51f47c7fec32553f873fa7f81a1baa3501b3b5ae13c99e13b45e6644aea78c0bb83c59c960cecd938fe4bebfa4181e7c253f68ad5709972942e36682cf305db6747622c54a70c6786733,
	<<Num:Size>>;
getPrime(2048 = Size) ->
	%true
	Num = 16#00925a3317e544e9f7d467ae16727b9d5c07559ce3c2dc37e8b3bac419788093df6bb5c26f2fc9378b90a8c5b7f9f50fc72449cc588605e6a2b60bf82f9a143f7846edb9e78548f51b438140e0351fadd7510c3d893e2099dadb01db010887a62a4e620f625c2ef50419b5dac3ff67cb883232e7ee5598e91fd0395ea1a2916d0544887ff7a0d6899632416543f5b597787483faf2328e42cdfb0f7029aa0cf4f32c257ffca630d3fc36e6078bef4d98f1c4c51cedbb5c722d61a8a2f189cc7b14b65eabc7578cc8baf095687f4e814198c8e877558c8f287e275a8c72f49bc04c107577608034a37e681219b6b94a8ce6f1b10d1df8e071af49f7589a72b5d763,
	<<Num:Size>>.





getScrambler() ->
	ServerPublic = getServerPublic(),
	ClientPublic = getClientPublic(),
	hash([ClientPublic, ServerPublic]).


getVerifier() ->
	G = getGenerator(),
	P = getPrime(),
	DerivedKey = getDerivedKey(),
	crypto:mod_pow(G, DerivedKey, P).


getClientPrivate() ->
	Size = getSize(),
	Key = {client_priv, Size},
	Val = get(Key),
	case Val of
		undefined ->
			NewVal = crypto:rand_bytes(Size div 8),
			put(Key, NewVal),
			NewVal;
		Value -> Value
	end.


getServerPrivate2() -> <<169,208,153,249,164,236,222,68,90,81,39,34,231,97,28,116,134,
               135,173,43,165,6,229,22,85,208,211,92,62,207,181,246>>.
getServerPrivate() ->
	Size = getSize(),
	Key = {server_priv, Size},
	Val = get(Key),
	case Val of
		undefined ->
			NewVal = crypto:rand_bytes(Size div 8),
			put(Key, NewVal),
			NewVal;
		Value -> Value
	end.

getDerivedKey() ->
	%I = getUsername(),
	P = getPassword(),
	Salt = getSalt(),
	%hash([Salt, hash([I, <<$:>>, P])]).
	hash([Salt, P]).

hash(L) ->
	crypto:hash(sha512, L).



%% public
getClientPublic() ->
	Priv = getClientPrivate(),
	G = getGenerator(),
	P = getPrime(),
	Version = getVersion(),
	{Pub, Priv} = crypto:generate_key(srp, {user, [G, P, Version]}, Priv),
	%% manually verify
	Pub = crypto:mod_pow(G, Priv, P),
	Pub.

getServerPublic() ->
	Priv = getServerPrivate(),
	G = getGenerator(),
	P = getPrime(),
	K = getMult(),
	Version = getVersion(),
	V = getVerifier(),
	{Pub, Priv} = crypto:generate_key(srp, {host, [V, G, P, Version]}, Priv),
	%% manually verify
	KVNum = mod( ( int(K) * int(V) ), int(P)),
	GBNum = int( crypto:mod_pow(G, Priv, P) ),
	Size = getSize(),
	Pub = <<( mod( GBNum + KVNum, int(P)) ):Size/integer>>,
	Pub.


computeClientKey() ->
	ServerPublic = getServerPublic(),
	ClientPrivate = getClientPrivate(),
	ClientPublic = getClientPublic(),
	G = getGenerator(),
	P = getPrime(),
	U = getScrambler(),
	Version = getVersion(),
	DerivedKey = getDerivedKey(),
	crypto:compute_key(srp, ServerPublic, {ClientPublic, ClientPrivate}, {user, [DerivedKey, P, G, Version, U]}).

computeServerKey() ->
	ClientPublic = getClientPublic(),
	ServerPrivate = getServerPrivate(),
	ServerPublic = getServerPublic(),
	P = getPrime(),
	U = getScrambler(),
	Version = getVersion(),
	V = getVerifier(),
	crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate}, {host, [V, P, Version, U]}).

computeClientKeyManually() ->
	P = getPrime(),
	ClientPrivate = getClientPrivate(),
	ServerPublic = getServerPublic(),
	U = getScrambler(),
	X = getDerivedKey(),
	Size = getSize(),
	V = getVerifier(),
	K = getMult(),

	KGXNum = mod( (int(K) * int(V)), int(P)),
	BaseNum = mod( (int(ServerPublic) - KGXNum ), int(P) ),
	Base = <<BaseNum:Size/integer>>,

	UXNum = mod( ( int(U) * int(X) ), int(P)),
	AUXNum = mod( ( int(ClientPrivate) + UXNum ), int(P) ),
	Exp = <<AUXNum:Size/integer>>,

	%io:format("kgxnum: ~p~nbasenum: ~p~nbase: ~p~n~nuxnum: ~p~nauxnum: ~p~nexp: ~p~n~n", [KGXNum, BaseNum, Base, UXNum, AUXNum, Exp]),

	crypto:mod_pow(Base, Exp, P).

computeServerKeyManually() ->
	P = getPrime(),
	ClientPublic = getClientPublic(),
	ServerPrivate = getServerPrivate(),
	U = getScrambler(),
	V = getVerifier(),
	BaseBin = crypto:mod_pow(V, U, P),
	BaseNum = mod((int(ClientPublic) * int(BaseBin)), int(P)),
	Size = getSize(),
	Base = <<BaseNum:Size/integer>>,
	crypto:mod_pow(Base, ServerPrivate, P).


testClientKey() ->
	SKeyManual = computeClientKeyManually(),
	SKey = computeClientKey(),
	io:format("skey manual: ~p~nskey lib: ~p~n", [SKeyManual, SKey]),
	SKeyManual == SKey.
testServerKey() ->
	SKeyManual = computeServerKeyManually(),
	SKey = computeServerKey(),
	io:format("skey manual: ~p~nskey lib: ~p~n", [SKeyManual, SKey]),
	SKeyManual == SKey.

testBoth() ->
	erase(),
	BoolServer = testServerKey(),
	if not BoolServer ->
		io:format("Server keys do not match~n");
		true -> ok
	end,
	BoolClient = testClientKey(),
	if not BoolClient ->
		%ClientPrivate = getClientPrivate(),
		%ServerPrivate = getServerPrivate(),
		%io:format("client priv: ~p~n", [ClientPrivate]),
		%io:format("server priv: ~p~n", [ServerPrivate]),
		io:format("Client keys do not match~n");
		true -> ok
	end,
	M1 = BoolClient andalso BoolServer,
	M2 = computeServerKey() == computeClientKey(),
	if not M2 ->
		io:format("Server key does match client key~n");
		true -> ok
	end,
	{M1, M2}.




int(Bin) ->
	crypto:bytes_to_integer(Bin).
mod(Num, P) -> Num rem P.

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
	P = getPrime(),
	ClientKey = computeClientKey(),
	ServerKey = computeServerKey(),
	Prime = size(P),
	io:format("prime is ~p bytes ~n", [Prime]),
	hash(ClientKey) == hash(ServerKey).
