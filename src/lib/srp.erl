-module(srp).
-export([test/0]).
-compile([export_all]).

-include("include/binary.hrl").


getUsername() -> <<"alice">>.

getPassword() -> <<"password123">>.

getTestSalt() -> <<16#b3d47dc40109ba25459096abdd0bfbbc3266d5b2dcf52eb586d2d2e612afdd84?QQ>>.

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
	PassHash = crypto:hash(sha, [UNormBin, <<$:>>, PwNormBin]),
	X = crypto:hash(sha, [Salt, PassHash]),
	l_to_b_endian(X, 160).

normalize(BinString) ->
	S1 = binary_to_list(BinString),
	S2 = string:to_upper(S1),
	list_to_binary(S2).

getScrambler(ClientPublic, ServerPublic) ->
	ClientPublicL = b_to_l_endian(ClientPublic, 256),
	ServerPublicL = b_to_l_endian(ServerPublic, 256),
	U = crypto:hash(sha, [ClientPublicL, ServerPublicL]),
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



testM() ->
	%% little endian
	%ClientPublicLittle = <<16#8985507fe6263740873c3605a4d6507592ad02095ac88e871ab623ac9f5bb388:256/unsigned-little-integer>>,
	ClientPublicLittle = <<16#0f6621dd4a39e4df6e9b2d07e8169eb0d33c917276bdbb1eeefc61f20f809649?QQ>>,
	%ClientM1Little = <<16#c098171e12b60dc72d64eaa63614e5dff07ce1cf:160/unsigned-little-integer>>,
	ClientM1Little = <<16#2591d18d14931a22a5751d172313ccb80d4232f4?SH>>,
	ClientPublic = ClientPublicLittle,
	ClientM1 = ClientM1Little,

	Generator = getGenerator(),
	Prime = getPrime(),
	I = getUsername(),
	Salt = getTestSalt(),
	Pw = getPassword(),
	ServerPrivate = <<116,251,24,248,115,211,4,76,142,129,49,189,104,186,81,185,50,182,209,247,131,98,164,163,244,125,158,200,101,214,37,146>>,
	DerivedKey = getDerivedKey(I, Pw, Salt),
	ServerPublic = getServerPublic(Generator, Prime, ServerPrivate, DerivedKey),

	Skey = srp:computeServerKey(ServerPrivate, ClientPublic, ServerPublic, Generator, Prime, DerivedKey),
	io:format("server skey: ~p~n", [Skey]),
	Key = srp:interleaveHash(Skey),

	io:format("X: ~p~n", [DerivedKey]),
	io:format("client pub: ~p~n~nserver pub: ~p~n~n", [ClientPublic, ServerPublic]),
	io:format("I: ~p~n~nPrime: ~p~n~nGen: ~p~n~nSalt: ~p~n~n", [I, Prime, Generator, Salt]),
	ServerM1 = srp:getM1(Prime, Generator, I, Salt, ClientPublic, ServerPublic, Key),
	io:format("m1 server: ~p~n~nm1 client: ~p~n~n", [ServerM1, ClientM1]),
	ServerM1 == ClientM1.



interleaveHash(Input) ->
	InputL = b_to_l_endian(Input, 256),
	%% todo remove all zero bytes from beginning of input
	{E, F} = getBytes(InputL, {[], []}, 0),
	G = hash(E),
	H = hash(F),
	Out = combineHashes(G, H, []),
	%io:format("input: ~p~nE: ~p~nF: ~p~nG: ~p~nH: ~p~nout: ~p~n", [Input, E, F, G, H, Out]),
	OutBin = iolist_to_binary(Out),
	l_to_b_endian(OutBin, 320).

getBytes(<<>>, Data, _N) -> Data;
getBytes(<<T?B, Rest/binary>>, {E, F}, N) ->
	IsEven = N rem 2 == 0,
	NewData = if IsEven ->
			NewE = E ++ [T],
			{NewE, F};
		true ->
			NewF = F ++ [T],
			{E, NewF}
		end,
	getBytes(Rest, NewData, N+1).

combineHashes(<<>>, <<>>, Data) -> Data;
combineHashes(<<G0?B, GRest/binary>>, <<H0?B, HRest/binary>>, Data) ->
	NewData = Data ++ [G0] ++ [H0],
	combineHashes(GRest, HRest, NewData).



test() ->
	%% these session keys should match
	G = getGenerator(),
	P = getPrime(),
	U = getUsername(),
	Pw = getPassword(),
	Salt = <<239,27,221,246,114,96,221,34,69,81,89,69,2,149,254,80,151,158,153,201,136,9,145,38,2,169,119,228,227,174,159,220>>,
	DerivedKey = getDerivedKey(U, Pw, Salt),
	Verifier = getVerifier(G, P, DerivedKey),
	Verifier = <<84,14,176,38,65,71,128,194,248,49,15,114,126,147,21,11,135,75,92,127,27,50,130,135,187,94,109,209,234,99,173,223>>,

	ClientPrivate = <<253,115,16,39,52,87,177,71,73,100,222,219,213,230,228,201,137,27,18,106,252,205,236,212,107,177,235,23,175,224,72,158>>,
	ServerPrivate = <<86,103,234,37,43,208,155,3,195,133,252,143,192,89,104,231,9,243,28,252,184,124,48,244,160,131,167,8,127,144,202,93>>,

	io:format("prime: ~p~n", [P]),
	ServerPublic = getServerPublic(G, P, ServerPrivate, Verifier),
	ClientPublic = getClientPublic(G, P, ClientPrivate),
	io:format("client pub: ~p~n", [ClientPublic]),
	io:format("server pub: ~p~n", [ServerPublic]),

	ClientKey = computeClientKey(ClientPrivate, ServerPublic, ClientPublic, G, P, DerivedKey),
	ServerKey = computeServerKey(ServerPrivate, ClientPublic, ServerPublic, P, Verifier),

	io:format("client skey: ~p~n", [ClientKey]),
	io:format("server skey: ~p~n", [ServerKey]),
	ClientKey == ServerKey.

doTest() ->
	U = getUsername(),
	Pw = getPassword(),
	Salt = getTestSalt(),
	X = getDerivedKey(U, Pw, Salt),
	%Xhex = bin_to_hex_list(X),
	%<<Xint?SHB>> = X,
	%io:format("Xhex: ~p~nXint: ~p~n", [Xhex, Xint]),
	P = getPrime(),
	%Phex = bin_to_hex_list(P),
	%<<Pint?QQB>> = P,
	%io:format("P: ~p~nPhex: ~p~nPint: ~p~n", [P, Phex, Pint]),
	G = getGenerator(),
	%V = getVerifier(G, P, X),
	%Vhex = bin_to_hex_list(V),
	%<<Vint?QQB>> = V,
	%io:format("Vhex: ~p~nVint: ~p~n", [Vhex, Vint]),
	ServerPrivate = <<16#74FB18F873D3044C8E8131BD68BA51B932B6D1F78362A4A3F47D9EC865D62592?QQB>>,
	%ServerPrivatehex = bin_to_hex_list(ServerPrivate),
	%<<ServerPrivateint?QQB>> = ServerPrivate,
	%io:format("ServerPrivate: ~p~nServerPrivatehex: ~p~nServerPrivateint: ~p~n", [ServerPrivate, ServerPrivatehex, ServerPrivateint]),
	ServerPublic = getServerPublic(G, P, ServerPrivate, X),
	Bhex = bin_to_hex_list(ServerPublic),
	<<Bint?QQB>> = ServerPublic,
	io:format("B: ~p~nBhex: ~p~nBint: ~p~n", [ServerPublic, Bhex, Bint]),

	ClientPublic = <<16#63547EDF0AB99A777D094B3BA93EC6739B3851D7CF956B7EF21BEE3846F6A3D5?QQB>>,
	%ClientPublic = l_to_b_endian(ClientPublicL, 256),
	Ahex = bin_to_hex(ClientPublic),
	<<Aint?QQB>> = ClientPublic,
	%ClientPublicL = <<Aint?QQ>>,

	M1 = <<16#487AC99DB5E745D79F55CCCBCD44ED989FF2A65?SHB>>,
	%M1 = l_to_b_endian(M1L, 160),
	<<M1num?SHB>> = M1,
	Mhex = bin_to_hex_list(M1),
	io:format("A: ~p~nAhex: ~p~nAint: ~p~n", [ClientPublic, Ahex, Aint]),
	io:format("M: ~p~nMhex: ~p~nMint: ~p~n", [M1, Mhex, M1num]),

	Verifier = getVerifier(G, P, X),
	Skey = computeServerKey(ServerPrivate, ClientPublic, ServerPublic, P, Verifier),
	%<<Skeyint?QQB>> = Skey,
	%Skeyhex = bin_to_hex_list(Skey),
	%io:format("skey: ~p~nskey hex: ~p~nskey int: ~p~n", [Skey, Skeyhex, Skeyint]),

	%Skeyl = <<Skeyint?QQ>>,
	if byte_size(Skey) == 31 ->
		io:format("skey is only 31 bits: ~p~nserver priv: ~p~nserver pub: ~p~nclient pub: ~p~n", [Skey, ServerPrivate, ServerPublic, ClientPublic]);
		true -> ok
	end,
	Key = srp:interleaveHash(Skey),
	<<Keyint?SLB>> = Key,
	%Keyl = <<Keyint?SL>>,
	Keyhex = bin_to_hex_list(Key),
	io:format("key: ~p~nkey hex: ~p~nkey int: ~p~n", [Key, Keyhex, Keyint]),

	encryptHeader(Key),


	%<<Pnum?QQB>> = P,
	%Pl = <<Pnum?QQ>>,
	%Pl = b_to_l_endian(P, 256),
	%Bl = <<Bint?QQ>>,
	M1Server = getM1(P, G, U, Salt, ClientPublic, ServerPublic, Key),
	M1serverhex = bin_to_hex_list(M1Server),
	<<M1Serverint?SHB>> = M1Server,
	%io:format("Ms: ~p~nMshex: ~p~nMsint: ~p~n", [M1Server, M1serverhex, M1Serverint]),

	M2 = getM2(ClientPublic, M1, Key),
	<<M2Int?SHB>> = M2,
	M2Hex = bin_to_hex_list(M2),
	%io:format("M2: ~p~nM2hex: ~p~nM2int: ~p~n", [M2, M2Hex, M2Int]),
	ok.

	encryptHeader(Key) ->
		KeyL = b_to_l_endian(Key, 320),
		KeyLList = binary_to_list(KeyL),
		KeyState = {0,0,KeyLList},

		Header = <<0, 6, 236, 1>>,

		{EncHeader1, NewKeyState} = world_crypto:encrypt(Header, KeyState),
		EncHeader = binary_to_list(EncHeader1),
		lists:foreach(fun(El) -> io:format("element: ~p~n", [El]) end, EncHeader),
		io:format("old header: ~p~nnew header: ~p~n", [Header, EncHeader]),

		ok.




	testU() ->
		I = getUsername(),
		Pw = getPassword(),
		Salt = getTestSalt(),
		X = getDerivedKey(I, Pw, Salt),

		P = getPrime(),
		G = getGenerator(),

		ServerPrivate = <<16#74FB18F873D3044C8E8131BD68BA51B932B6D1F78362A4A3F47D9EC865D62592?QQB>>,
		ServerPublic = getServerPublic(G, P, ServerPrivate, X),

		Al = <<16#090fb4ad2d9529a293b17109502cb2cbccf0e45aa2ab7a0f642f67b0b98a2237?QQ>>,
		<<Anum?QQB>> = Al,
		ClientPublic = <<Anum?QQB>>,

		U = getScrambler(ClientPublic, ServerPublic),

		Uhex = bin_to_hex_list(U),
		<<Uint?SHB>> = U,
		io:format("U: ~p~nUhex: ~p~nUint: ~p~n", [U, Uhex, Uint]),
		ok.


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


padded(V, N) when byte_size(V) =:= byte_size(N) -> V;
padded(V, N) when byte_size(V) < byte_size(N) ->
	NLen = byte_size(N),
	PadLen = (NLen - byte_size(V)) * 8,
	Pad = <<0:PadLen>>,
	[Pad, V].

bin_to_hex_list(Bin) when is_binary(Bin) ->
  lists:flatten([integer_to_list(X,16) || <<X>> <= Bin]).

	bin_to_hex(B) when is_binary(B) ->
  bin_to_hex(B, <<>>).

-define(H(X), (hex(X)):16).

bin_to_hex(<<>>, Acc) -> Acc;
bin_to_hex(Bin, Acc) when byte_size(Bin) band 7 =:= 0 ->
  bin_to_hex_(Bin, Acc);
bin_to_hex(<<X:8, Rest/binary>>, Acc) ->
  bin_to_hex(Rest, <<Acc/binary, ?H(X)>>).

bin_to_hex_(<<>>, Acc) -> Acc;
bin_to_hex_(<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>>, Acc) ->
  bin_to_hex_(
    Rest,
    <<Acc/binary,
      ?H(A), ?H(B), ?H(C), ?H(D), ?H(E), ?H(F), ?H(G), ?H(H)>>).
		
-compile({inline, [hex/1]}).

hex(X) ->
  element(
    X+1, {16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036,
          16#3037, 16#3038, 16#3039, 16#3041, 16#3042, 16#3043, 16#3044,
          16#3045, 16#3046, 16#3130, 16#3131, 16#3132, 16#3133, 16#3134,
          16#3135, 16#3136, 16#3137, 16#3138, 16#3139, 16#3141, 16#3142,
          16#3143, 16#3144, 16#3145, 16#3146, 16#3230, 16#3231, 16#3232,
          16#3233, 16#3234, 16#3235, 16#3236, 16#3237, 16#3238, 16#3239,
          16#3241, 16#3242, 16#3243, 16#3244, 16#3245, 16#3246, 16#3330,
          16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337,
          16#3338, 16#3339, 16#3341, 16#3342, 16#3343, 16#3344, 16#3345,
          16#3346, 16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435,
          16#3436, 16#3437, 16#3438, 16#3439, 16#3441, 16#3442, 16#3443,
          16#3444, 16#3445, 16#3446, 16#3530, 16#3531, 16#3532, 16#3533,
          16#3534, 16#3535, 16#3536, 16#3537, 16#3538, 16#3539, 16#3541,
          16#3542, 16#3543, 16#3544, 16#3545, 16#3546, 16#3630, 16#3631,
          16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637, 16#3638,
          16#3639, 16#3641, 16#3642, 16#3643, 16#3644, 16#3645, 16#3646,
          16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736,
          16#3737, 16#3738, 16#3739, 16#3741, 16#3742, 16#3743, 16#3744,
          16#3745, 16#3746, 16#3830, 16#3831, 16#3832, 16#3833, 16#3834,
          16#3835, 16#3836, 16#3837, 16#3838, 16#3839, 16#3841, 16#3842,
          16#3843, 16#3844, 16#3845, 16#3846, 16#3930, 16#3931, 16#3932,
          16#3933, 16#3934, 16#3935, 16#3936, 16#3937, 16#3938, 16#3939,
          16#3941, 16#3942, 16#3943, 16#3944, 16#3945, 16#3946, 16#4130,
          16#4131, 16#4132, 16#4133, 16#4134, 16#4135, 16#4136, 16#4137,
          16#4138, 16#4139, 16#4141, 16#4142, 16#4143, 16#4144, 16#4145,
          16#4146, 16#4230, 16#4231, 16#4232, 16#4233, 16#4234, 16#4235,
          16#4236, 16#4237, 16#4238, 16#4239, 16#4241, 16#4242, 16#4243,
          16#4244, 16#4245, 16#4246, 16#4330, 16#4331, 16#4332, 16#4333,
          16#4334, 16#4335, 16#4336, 16#4337, 16#4338, 16#4339, 16#4341,
          16#4342, 16#4343, 16#4344, 16#4345, 16#4346, 16#4430, 16#4431,
          16#4432, 16#4433, 16#4434, 16#4435, 16#4436, 16#4437, 16#4438,
          16#4439, 16#4441, 16#4442, 16#4443, 16#4444, 16#4445, 16#4446,
          16#4530, 16#4531, 16#4532, 16#4533, 16#4534, 16#4535, 16#4536,
          16#4537, 16#4538, 16#4539, 16#4541, 16#4542, 16#4543, 16#4544,
          16#4545, 16#4546, 16#4630, 16#4631, 16#4632, 16#4633, 16#4634,
          16#4635, 16#4636, 16#4637, 16#4638, 16#4639, 16#4641, 16#4642,
          16#4643, 16#4644, 16#4645, 16#4646}).
