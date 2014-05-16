-module(logon_lib).
-export([getUsername/0, getPassword/0, getClientPrivate/0, getServerPrivate/0, getSalt/0, getGenerator/0, getPrime/0]).
-export([getClientPublic/0, getServerPublic/0, computeClientKey/0, computeServerKey/0]).
-export([getM/3, hash/1, getVersion/0]).
-export([test/0]).

-compile([export_all]).

-include("include/binary.hrl").



%% number of bits we are using
%getSize() -> 256.
getSize() -> 256.





getUsername() ->
	<<"alice">>.

getPassword() ->
	<<"password123">>.


%getSalt() -> <<16#b3d47dc40109ba25459096abdd0bfbbc3266d5b2dcf52eb586d2d2e612afdd84:256>>.
%getSalt() -> <<"mystrongsalt">>.
getSalt() ->
	<<"mys">>.
%hexstr2bin("BEB25379D1A8581EB5A727673A2441EE").

getMult() ->
	Version = getVersion(),
	P = getPrime(),
	case Version of
				'6' ->
						crypto:mod_pow(<<3>>, <<1>>, P);
				'6a' ->
					Generator = getGenerator(),
					GPadded = srp_pad_to(erlang:byte_size(P), Generator),
					hash([P, GPadded])
			end.

srp_pad_length(Width, Length) ->
    (Width - Length rem Width) rem Width.

srp_pad_to(Width, Binary) ->
    case srp_pad_length(Width, size(Binary)) of
        0 -> Binary;
        N -> << 0:(N*8), Binary/binary>>
    end.

getGenerator() ->
	P = getPrime(),
	crypto:mod_pow(<<7>>, <<1>>, P).

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
	Num = 16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7,
	%Num = 16#f81217149d53d50d4a8adb0bbf57a5631d5d16218971c9b419f27f3828e98ac3,
	<<Num:Size>>;
getPrime(512 = Size) ->
	%true
	Num = 16#008253ea781be9e6423746924c4b16c6ea9bfdf51abb67bebf1c1259bbba1e8a8b5141eb5fc1d573e5915c065b81b23d0f14ff634ddb92fd73a110a284c6c74aeb,
	<<Num:Size>>;
getPrime(1024 = Size) ->
    P = hexstr2bin("EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE48E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B297BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9AFD5138FE8376435B9FC61D2FC0EB06E3"),
		Num = 16#EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE48E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B297BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9AFD5138FE8376435B9FC61D2FC0EB06E3,
		P = <<Num:Size>>,
		P;
	%true
	%Num = 16#008e3a4c67a5ac5243a69f856dd3e12b2694c0f5f9a1956f5b3b69b83574cc47713146a981e496280290fe3dd7f231bf2328bedbd8b90d51f47c7fec32553f873fa7f81a1baa3501b3b5ae13c99e13b45e6644aea78c0bb83c59c960cecd938fe4bebfa4181e7c253f68ad5709972942e36682cf305db6747622c54a70c6786733,
	%<<Num:Size>>;
getPrime(2048 = Size) ->
	%true
	Num = 16#00925a3317e544e9f7d467ae16727b9d5c07559ce3c2dc37e8b3bac419788093df6bb5c26f2fc9378b90a8c5b7f9f50fc72449cc588605e6a2b60bf82f9a143f7846edb9e78548f51b438140e0351fadd7510c3d893e2099dadb01db010887a62a4e620f625c2ef50419b5dac3ff67cb883232e7ee5598e91fd0395ea1a2916d0544887ff7a0d6899632416543f5b597787483faf2328e42cdfb0f7029aa0cf4f32c257ffca630d3fc36e6078bef4d98f1c4c51cedbb5c722d61a8a2f189cc7b14b65eabc7578cc8baf095687f4e814198c8e877558c8f287e275a8c72f49bc04c107577608034a37e681219b6b94a8ce6f1b10d1df8e071af49f7589a72b5d763,
	<<Num:Size>>.





getScrambler() ->
	ServerPublic = getServerPublic(),
	ClientPublic = getClientPublic(),
	%Scrambler = hexstr2bin("CE38B9593487DA98554ED47D70A7AE5F462EF019"),
	Scrambler = hash([ClientPublic, ServerPublic]),
			%Scrambler = hexstr2bin("02E2476A"),
	Scrambler.


getVerifier() ->
	G = getGenerator(),
	P = getPrime(),
	DerivedKey = getDerivedKey(),
	V = crypto:mod_pow(G, DerivedKey, P),
	%V = hexstr2bin("7E273DE8696FFC4F4E337D05B4B375BEB0DDE1569E8FA00A9886D8129BADA1F1822223CA1A605B530E379BA4729FDC59F105B4787E5186F5C671085A1447B52A48CF1970B4FB6F8400BBF4CEBFBB168152E08AB5EA53D15C1AFF87B2B9DA6E04E058AD51CC72BFC9033B564E26480D78E955A5E29E7AB245DB2BE315E2099AFB"),
	V.



getClientPrivate() ->
	%<<169,208,153,249,164,236,222,68,90,81,39,34,231,97,28,116,134,
               %135,173,43,165,6,229,22,85,208,211,92,62,207,181,246>>.
	%hexstr2bin("60975527035CF2AD1989806F0407210BC81EDC04E2762A56AFD529DDDA2D4393").
	<<16#149F832EE8D67ECF9E7F2785EB0622D8B3FE2344C00F96E1AEF4103CA44D51F9:256>>.
getClientPrivate2() ->
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


getServerPrivate() ->
	%<<169,208,153,249,164,236,222,68,90,81,39,34,231,97,28,116,134,
               %135,173,43,165,6,229,22,85,208,211,92,62,207,181,246>>.
	%hexstr2bin("E487CB59D31AC550471E81F00F6928E01DDA08E974A004F49E61F5D105284D20").
	%<<16#6C78CCEAAEC15E69068A87795B2A20ED7B45CFC5A254EBE2F17F144A4D99DB18:256>>.
	<<16#8C78CCEAAEC15E69068A87795B2A20ED7B45CFC5A254EBE2F17F144A4D99DB18:256>>.
getServerPrivate2() ->
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
	I = getUsername(),
	P = getPassword(),
	Salt = getSalt(),
	X = hash([Salt, hash([I, <<$:>>, P])]),
	X = crypto:hash(sha, [Salt, crypto:hash(sha, [I, <<$:>>, P])]),
	D = crypto:hash_init(sha),
	D2 = crypto:hash_update(D, [I, <<$:>>, P]),
	X1 = crypto:hash_final(D2),
	D3 = crypto:hash_init(sha),
	D4 = crypto:hash_update(D3, [Salt, X1]),
	X = crypto:hash_final(D4),
	X.
	%hash([Salt, P]).

hash(L) ->
	crypto:hash(sha, L).



%% public
getClientPublic() ->
	Priv = getClientPrivate(),
	G = getGenerator(),
	P = getPrime(),
	Version = getVersion(),
	{Pub, Priv} = crypto:generate_key(srp, {user, [G, P, Version]}, Priv),
	%% manually verify
	Pub = crypto:mod_pow(G, Priv, P),
	%Pub = hexstr2bin("61D5E490F6F1B79547B0704C436F523DD0E560F0C64115BB72557EC44352E8903211C04692272D8B2D1A5358A2CF1B6E0BFCF99F921530EC8E39356179EAE45E42BA92AEACED825171E1E8B9AF6D9C03E1327F44BE087EF06530E69F66615261EEF54073CA11CF5858F0EDFDFE15EFEAB349EF5D76988A3672FAC47B0769447B"),
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

	KVNum = ( int(K) * int(V) ),
	KVNumMod = mod( KVNum, int(P)),
	GBNum = int( crypto:mod_pow(G, Priv, P) ),
	Size = getSize(),
	SumNum = mod( GBNum + KVNumMod, int(P)),
	Pub = <<( SumNum ):Size/integer>>,

	%Pub = hexstr2bin("BD0C61512C692C0CB6D041FA01BB152D4916A1E77AF46AE105393011BAF38964DC46A0670DD125B95A981652236F99D9B681CBF87837EC996C6DA04453728610D0C6DDB58B318885D7D82C7F8DEB75CE7BD4FBAA37089E6F9C6059F388838E7A00030B331EB76840910440B1B27AAEAEEB4012B7D7665238A8E3FB004B117B58"),
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
	Skey = crypto:compute_key(srp, ServerPublic, {ClientPublic, ClientPrivate}, {user, [DerivedKey, P, G, Version, U]}),
	%Skey = hexstr2bin("B0DC82BABCF30674AE450C0287745E7990A3381F63B387AAF271A10D233861E359B48220F7C4693C9AE12B0A6F67809F0876E2D013800D6C41BB59B6D5979B5C00A172B4A2A5903A0BDCAF8A709585EB2AFAFA8F3499B200210DCC1F10EB33943CD67FC88A2F39A4BE5BEC4EC0A3212DC346D7E474B29EDE8A469FFECA686E5A"),
	Skey.

computeServerKey() ->
	ClientPublic = getClientPublic(),
	ServerPrivate = getServerPrivate(),
	ServerPublic = getServerPublic(),
	P = getPrime(),
	U = getScrambler(),
	Version = getVersion(),
	V = getVerifier(),
	Skey = crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate}, {host, [V, P, Version, U]}),
	%Skey = hexstr2bin("B0DC82BABCF30674AE450C0287745E7990A3381F63B387AAF271A10D233861E359B48220F7C4693C9AE12B0A6F67809F0876E2D013800D6C41BB59B6D5979B5C00A172B4A2A5903A0BDCAF8A709585EB2AFAFA8F3499B200210DCC1F10EB33943CD67FC88A2F39A4BE5BEC4EC0A3212DC346D7E474B29EDE8A469FFECA686E5A"),
	Skey.

computeClientKeyManually() ->
	P = getPrime(),
	ClientPrivate = getClientPrivate(),
	ServerPublic = getServerPublic(),
	U = getScrambler(),
	X = getDerivedKey(),
	Size = getSize(),
	K = getMult(),
	G = getGenerator(),
	V = getVerifier(),

	%% S = (B - kg^x) ^ (a + ux)
	KGXNum = mod( (int(K) * int(V)), int(P)),
	GX = crypto:mod_pow(G, X, P),
	KGXNum = mod( (int(K) * int(GX)), int(P)),
	io:format("bint: ~p~nkgxn: ~p~n", [int(ServerPublic), KGXNum]),

	Sum = (int(ServerPublic) - KGXNum ),

	BaseNum = mod( Sum, int(P) ),
	%io:format("sum: ~p~nn: ~p~nbasenum: ~p~n", [Sum, int(P), BaseNum]),
	Base = crypto:mod_pow(<<Sum:Size/integer>>, <<1>>, P),
	Base = <<BaseNum:Size/integer>>,

	UXNum = mod( ( int(U) * int(X) ), int(P)),
	AUXNum = mod( ( int(ClientPrivate) + UXNum ), int(P) ),
	Exp = <<AUXNum:Size/integer>>,

	%io:format("kgxnum: ~p~nbasenum: ~p~nbase: ~p~n~nuxnum: ~p~nauxnum: ~p~nexp: ~p~n~n", [KGXNum, BaseNum, Base, UXNum, AUXNum, Exp]),

	SkeyNum = crypto:mod_exp(BaseNum, AUXNum, int(P)),
	Skey = <<SkeyNum:Size/integer>>,
	Skey = crypto:mod_pow(Base, Exp, P),

	%% wrong
	%Skey = hexstr2bin("B0DC82BABCF30674AE450C0287745E7990A3381F63B387AAF271A10D233861E359B48220F7C4693C9AE12B0A6F67809F0876E2D013800D6C41BB59B6D5979B5C00A172B4A2A5903A0BDCAF8A709585EB2AFAFA8F3499B200210DCC1F10EB33943CD67FC88A2F39A4BE5BEC4EC0A3212DC346D7E474B29EDE8A469FFECA686E5A"),
	Skey.

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
	Skey = crypto:mod_pow(Base, ServerPrivate, P),
	%Skey = hexstr2bin("B0DC82BABCF30674AE450C0287745E7990A3381F63B387AAF271A10D233861E359B48220F7C4693C9AE12B0A6F67809F0876E2D013800D6C41BB59B6D5979B5C00A172B4A2A5903A0BDCAF8A709585EB2AFAFA8F3499B200210DCC1F10EB33943CD67FC88A2F39A4BE5BEC4EC0A3212DC346D7E474B29EDE8A469FFECA686E5A"),
	Skey.

testPubs() ->
	Apub = getClientPublic(),
	Bpub = getServerPublic(),
	{Bpub, Apub}.


testClientKey() ->
	SKeyManual = computeClientKeyManually(),
	SKey = computeClientKey(),
	io:format("client skey manual: ~p~nclient skey lib: ~p~n", [SKeyManual, SKey]),
	SKeyManual == SKey.
testServerKey() ->
	SKeyManual = computeServerKeyManually(),
	SKey = computeServerKey(),
	io:format("server skey manual: ~p~nserver skey lib: ~p~n", [SKeyManual, SKey]),
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
