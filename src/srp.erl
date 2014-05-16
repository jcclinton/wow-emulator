    -module(srp).
    -export([test/0]).
		-compile([export_all]).
    
    
    getUsername() -> <<"alice">>.
    
    getPassword() -> <<"password123">>.
    
    getSalt() -> <<"mystrongsalt">>.
    
    getGenerator() -> <<7>>.
    
    %% srp version 6
    getVersion() -> '6'.
    
    % randomly generated 32 byte number
    getClientPrivate() ->
			%hexstr2bin("6411DE75538BED8170677D577D0608F39112BC95B503C447EB6AC94549C75C7B").
			<<16#149F832EE8D67ECF9E7F2785EB0622D8B3FE2344C00F96E1AEF4103CA44D51F9:256>>.
    
    % randomly generated 32 byte number
    getServerPrivate() ->
			%hexstr2bin("85E44A6F694DBE676145DB245A045CD37C99F05C562C7840A31F270D9AADCF8B").
	    <<16#6C78CCEAAEC15E69068A87795B2A20ED7B45CFC5A254EBE2F17F144A4D99DB18:256>>.
    
    
    %% 32 byte prime number
    %% used in mangos: https://github.com/mangoszero/server/blob/master/src/realmd/AuthSocket.cpp#L190
    %% used in arcemu: http://arcemu.org/wiki/Server_Logon_Challenge
    getPrime() ->
	    <<16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7:256>>.
    
    
    
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
    
		getScrambler() ->
			ServerPublic = getServerPublic(),
			ClientPublic = getClientPublic(),
			crypto:hash(sha, [ClientPublic, ServerPublic]).
			%srp_scrambler(getVersion(), getClientPublic(), getServerPublic(), getPrime()).
			%hexstr2bin("02E2476A02E2476A02E2476A").
			%hexstr2bin("02E2476A").
			%hexstr2bin("0123456789ABCDEF0123012345").
			%hexstr2bin("01").
			%<<21,77,31,181,139,85,29,236,113,58,232,55,222,141,189,141,218,190,42,126>>.
    
    
    %% client public key
    getClientPublic() ->
	    PrivateKey = getClientPrivate(),
	    Generator = getGenerator(),
	    Prime = getPrime(),
	    Version = getVersion(),
	                                                                                                {Pub, PrivateKey} = crypto:generate_key(srp, {user, [Generator, Prime, Version]}, PrivateKey),
	    Pub.
    
    %% server public key
    getServerPublic() ->
	    PrivateKey = getServerPrivate(),
	    Generator = getGenerator(),
	    Prime = getPrime(),
	    Version = getVersion(),
	    Verifier = getVerifier(),
	    {Pub, PrivateKey} = crypto:generate_key(srp, {host, [Verifier, Generator, Prime, Version]}, PrivateKey),
	Pub.

    
    %% client session key
    computeClientKey_old() ->
	    ServerPublic = getServerPublic(),
	    ClientPrivate = getClientPrivate(),
	    ClientPublic = getClientPublic(),
			U = getScrambler(),
	    Generator = getGenerator(),
	    Prime = getPrime(),
	    Version = getVersion(),
	    DerivedKey = getDerivedKey(),
	    crypto:compute_key(srp, ServerPublic, {ClientPublic, ClientPrivate}, {user, [DerivedKey, Prime, Generator, Version, U]}).

		computeClientKey() ->
	    ServerPublic = getServerPublic(),
	    ClientPrivate = getClientPrivate(),
	    ClientPublic = getClientPublic(),
			U = getScrambler(),
	    Generator = getGenerator(),
	    Prime = getPrime(),
	    Version = getVersion(),
	    DerivedKey = getDerivedKey(),
			PrimeI = bin_to_int(Prime),
			Multiplier = getMultiplier(Version, Generator, Prime),
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
    computeServerKey() ->
	    ClientPublic = getClientPublic(),
	    ServerPrivate = getServerPrivate(),
	    ServerPublic = getServerPublic(),
			U = getScrambler(),
	    Prime = getPrime(),
	    Version = getVersion(),
	    Verifier = getVerifier(),
	    crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate}, {host, [Verifier, Prime, Version, U]}).



    test() ->
	    %% these session keys should match
	    ClientKey = computeClientKey(),
	    ServerKey = computeServerKey(),
	    io:format("client skey: ~p~n", [ClientKey]),
	    io:format("server skey: ~p~n", [ServerKey]),
    io:format("scrambler: ~p~n",
              [srp_scrambler(getVersion(), getClientPublic(),
                             getServerPublic(), getPrime())]),
	    ClientKey == ServerKey.

			% hexstr2bin
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
	
srp_pad_length(Width, Length) ->
    (Width - Length rem Width) rem Width.
 
srp_pad_to(Width, Binary) ->
    case srp_pad_length(Width, size(Binary)) of
        0 ->
            Binary;
        N -> << 0:(N*8), Binary/binary>>
    end.
 
srp_scrambler(_Version, UserPublic, HostPublic, Prime) ->
    %% SHA1(PAD(A) | PAD(B)) from http://srp.stanford.edu/design.html
    PadLength = erlang:byte_size(Prime),
    C0 = crypto:sha_init(),
    C1 = crypto:sha_update(C0, srp_pad_to(PadLength, UserPublic)),
    C2 = crypto:sha_update(C1, srp_pad_to(PadLength, HostPublic)),
    crypto:sha_final(C2).


	padded(V, N) when byte_size(V) =:= byte_size(N) -> V;
padded(V, N) when byte_size(V) < byte_size(N) ->
    NLen = byte_size(N),
    PadLen = (NLen - byte_size(V)) * 8,
    Pad = <<0:PadLen>>,
    [Pad, V].

int_to_bin(Int) ->
    Len0 = length(erlang:integer_to_list(Int, 16)),
    Len1 = Len0 + (Len0 rem 2),
    Bits = Len1 * 4,
    <<Int:Bits>>.

bin_to_int(Bin) ->
    Bits = byte_size(Bin) * 8,
    <<Val:Bits>> = Bin,
    Val.

getMultiplier('6a', Generator, Prime) ->
    crypto:hash(sha, [Prime, padded(Generator, Prime)]);
getMultiplier('6', _Generator, _Prime) ->
    <<3/integer>>;
getMultiplier('3', _Generator, _Prime) ->
    <<1/integer>>.
