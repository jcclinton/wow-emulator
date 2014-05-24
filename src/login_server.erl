-module(login_server).
-behavior(gen_server).

-record(state, {name, % players name
								socket, % the current socket
								identity,
								verifier,
								salt,
								server_private,
								client_public,
								server_public,
								m1
							 }).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


-include("include/binary.hrl").
-include("include/database_records.hrl").


start_link(ListenSocket) ->
	gen_server:start_link(?MODULE, ListenSocket, []).

init(ListenSocket) ->
	io:format("login SERVER: started~n"),
	gen_server:cast(self(), {accept, ListenSocket}),
	{ok, #state{}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast({accept, ListenSocket}, State) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	% start new acceptor
	login_server_sup:start_socket(),
	{noreply, State#state{socket=AcceptSocket}}.



handle_info({tcp, _Socket, <<0?B, Msg/binary>>}, State=#state{socket=Socket}) ->
	ok = inet:setopts(Socket, [{active, once}]),
	I = try extract_username(Msg) of
		_ -> extract_username(Msg)
	catch
		throw:zero_ilen -> gen_tcp:send(Socket, <<100>>),
											 io:format("zero_ilen!~n"),
											 <<"">>;
		throw:bad_i_size -> gen_tcp:send(Socket, <<200>>),
											 io:format("bad_i_size!~n"),
												<<"">>;
				_ -> <<"">>
	end,
	io:format("SERVER: received challenge with name: ~p~n", [I]),
	Generator = srp:getGenerator(),
	Prime = srp:getPrime(),
	Account = account:lookup(I),
	NewState = if Account == false ->
			%% todo send error response
			State;
		true ->
			ServerPrivate = srp:generatePrivate(),
			Verifier = Account#account.verifier,
			Salt = Account#account.salt,
			ServerPublic = srp:getServerPublic(Generator, Prime, ServerPrivate, Verifier),
			MsgOut = build_challenge_response(ServerPublic, Generator, Prime, Salt),
			%io:format("sending chal resp: ~p~n", [Msg]),
			gen_tcp:send(Socket, MsgOut),
			State#state{identity=I, server_public=ServerPublic, salt=Salt, verifier=Verifier, server_private=ServerPrivate}
	end,
	{noreply, NewState};
handle_info({tcp, _Socket, <<1?B, Msg/binary>>}, State=#state{socket=Socket, server_public=ServerPublic, server_private=ServerPrivate, verifier=Verifier, salt=Salt, identity=Name}) ->
	ok = inet:setopts(Socket, [{active, once}]),
	io:format("SERVER: received proof~n"),
	{ClientPublic, M1} = extract_proof(Msg),
	Prime = srp:getPrime(),
	Skey = srp:computeServerKey(ServerPrivate, ClientPublic, ServerPublic, Prime, Verifier),
	%io:format("server skey: ~p~n", [Skey]),
	Key = srp:interleaveHash(Skey),
	%KeySize = size(Key),
	%io:format("output key: ~p~nsize: ~p~n", [Key, KeySize]),
	%Key = srp:hash([Skey]),
	StringName = binary_to_list(Name),
	KeyL = srp:b_to_l_endian(Key, 320),
	ets:insert(connected_clients, {StringName, KeyL}),
	io:format("SERVER: sending proof response~n"),
	Generator = srp:getGenerator(),
	MsgOut = build_proof_response(Prime, Generator, Salt, M1, ClientPublic, ServerPublic, Key),
	gen_tcp:send(Socket, MsgOut),
	{noreply, State#state{client_public=ClientPublic, m1=M1}};
handle_info({tcp, _Socket, <<16?B, _Msg/binary>>}, State=#state{socket=Socket}) ->
	ok = inet:setopts(Socket, [{active, once}]),
	io:format("SERVER: received realmlist req~n"),
	MsgOut = build_realmlist_response(),
	gen_tcp:send(Socket, MsgOut),
	{noreply, State};
handle_info({tcp, _Socket, Msg}, State=#state{socket=Socket}) ->
	ok = inet:setopts(Socket, [{active, once}]),
	io:format("SERVER: received unknown tcp message: ~p~n", [Msg]),
	{noreply, State};
handle_info(upgrade, State) ->
	%% loads latest code
	?MODULE:handle_info(do_upgrade, State),
	{noreply, State};
handle_info(_Msg, State) ->
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, State) ->
	io:format("REALMD: closing connected realm_socket~n"),
	catch gen_tcp:close(State#state.socket),
	ok.


%% private

build_challenge_response(ServerPublic, G, N, Salt) ->
	GLen = erlang:byte_size(G),
	NLen = erlang:byte_size(N),
	%io:format("bsize: ~p~n", [erlang:byte_size(B)]),
	%io:format("gsize: ~p~n", [erlang:byte_size(G)]),
	%io:format("nsize: ~p~n", [erlang:byte_size(N)]),
	%io:format("ssize: ~p~n", [erlang:byte_size(S)]),
	Unk3 = <<16#0123456789ABCDEF?QH>>,

	%% convert from big endian to little endian
	<<ServerPubNum:256>> = ServerPublic,
	ServerPubLittle = <<ServerPubNum?QQ>>,

	<<Nnum:256>> = N,
	NLittle = <<Nnum?QQ>>,

	%<<SaltNum:256>> = Salt,
	%SaltLittle = <<SaltNum?QQ>>,
	%% salt is already little endian
	SaltLittle = Salt,

	Msg = [_Cmd = <<0?B>>,
					_Err = <<0?B>>,
					_Unk2 = <<0?B>>,
					ServerPubLittle,
					<<GLen?B>>,
					G,
					<<NLen?B>>,
					NLittle,
					SaltLittle,
					Unk3,
					_Unk4 = <<0?B>>
				],

	%Size = lists:foldl(fun(Elem, Acc) -> Acc + size(Elem) end, 0, Msg),
	%io:format("response size: ~p~n", [Size]),
	Msg.

build_proof_response(Prime, Generator, Salt, ClientM1, ClientPublic, ServerPublic, Key) ->
	I = srp:getUsername(),
	%io:format("client pub: ~p~n~nserver pub: ~p~n~n", [ClientPublic, ServerPublic]),
	%io:format("I: ~p~n~nPrime: ~p~n~nGen: ~p~n~nSalt: ~p~n~n", [I, Prime, Generator, Salt]),

	ServerM1 = srp:getM1(Prime, Generator, I, Salt, ClientPublic, ServerPublic, Key),
	%io:format("m1 server: ~p~n~nm1 client: ~p~n~n", [ServerM1, ClientM1]),
	if ClientM1 == ServerM1 -> ok;
		true -> io:format("CLIENTM1 and SERVERM1 do not match!~n")
	end,
	M2 = srp:getM2(ClientPublic, ServerM1, Key),
	<<M2Num?SHB>> = M2,
	M2Little = <<M2Num?SH>>,
	%io:format("m2: ~p~n~n", [M2]),

	Msg = [_Cmd = <<1?B>>,
				 _Err = <<0?B>>,
				 M2Little,
				 _Flags = <<0?B>>,
				 <<0?B>>,
				 <<0?B>>,
				 <<0?B>>],
	Msg.

build_realmlist_response() ->
	%{ok, Port} = application:get_env(world_port),
	%PortBin = binary_list_from_integer(Port),
	PortBin = [<<$8>>, <<$8>>, <<$9>>, <<$9>>],
	Ip = [<<$1?B>>,
				<<$2?B>>,
				<<$7?B>>,
				<<$.>>,
				<<$0?B>>,
				<<$.>>,
				<<$0?B>>,
				<<$.>>,
				<<$1?B>>,
				<<$:>>,
				PortBin,
				<<$\0>>
			 ],
	Realms = [
						_Icon = <<1?L>>,
						_Lock = <<0?B>>,
						%_Status = <<1?B>>,
						%_RealmId = <<1?L>>,
						_RealmName = [<<"cool realm">>, <<$\0>>],
						Ip,
						_Pop = <<1?L>>,
						_Chars = <<2?B>>,
						_TZ = <<1?B>>,
						_Unk = <<16#0?B>>
					 ],
					 
                %pkt << uint32(i->second.icon);              // realm type
                %pkt << uint8(realmflags);                   // realmflags
                %pkt << name;                                // name
                %pkt << i->second.address;                   // address
                %pkt << float(i->second.populationLevel);
                %pkt << uint8(AmountOfCharacters);
                %pkt << uint8(i->second.timezone);           // realm category
                %pkt << uint8(0x00);  

	Payload = [_Start = <<0?L>>,
				 _realms = <<1?B>>,
				 Realms,
				 <<2?W>>],
	Size = getSize(Payload),
	Msg = [_Cmd = <<16#10?B>>,
				_Size = <<Size?W>>,
				Payload],
	Msg.

getSize([]) -> 0;
getSize([Hd|Tail]) when is_binary(Hd) ->
	size(Hd) + getSize(Tail);
getSize([Hd|Tail]) ->
	getSize(Hd) + getSize(Tail).


%% example 1234 -> [<<1>>, <<2>>, <<3>>, <<4>>]
binary_list_from_integer(Int) ->
	binary_list_from_integer(Int, [], 10).

binary_list_from_integer(Int, List, Digit) ->
	TotalRem = Int rem Digit,
	Rem = TotalRem div (Digit div 10),
	List2 = [<<Rem?B>> | List],
	if TotalRem == Int -> List2;
		 TotalRem /= Int -> binary_list_from_integer(Int, List2, Digit * 10)
	end.


extract_username(Msg) ->
		<<_Err?B,
		_Size?W,
		_GameName?L,
		_V1?B,
		_V2?B,
		_V3?B,
		_Build?W,
		_Platform?L,
		_OS?L,
		_Country?L,
		_TzBias?L,
		_Ip?L,
		ILen?B,
		I/binary>> = Msg,
		%io:format("ilen: ~p~n", [ILen]),
		%io:format("i size: ~p~n", [erlang:bit_size(I)]),
		Size = ILen * 8,
		if ILen == 0 -> throw(zero_ilen);
			 true -> ok
		end,
		<<INum:Size/unsigned-big-integer, Rest/binary>> = I,
		%io:format("Rest size: ~p~n", [erlang:bit_size(Rest)]),
		%io:format("Rest: ~p~n", [Rest]),
		BList = binary_to_list(Rest),
		RestVal = if length(BList) > 0 -> length(BList);
								 true -> 0
							end,
		%io:format("Restval: ~p~n", [RestVal]),
		if RestVal =/= 0 -> throw(bad_i_size);
			 true -> ok
		end,
		<<INum:Size/unsigned-big-integer>>.


extract_proof(Msg) ->
	<<ClientPublic_raw?QQ,
		M1_raw?SH,
		_Crc_hash?SH,
		_Num_keys?B,
		_Unk?B>> = Msg,
	ClientPublic = <<ClientPublic_raw?QQB>>,
	M1 = <<M1_raw?SHB>>,
	%io:format("client public: ~p~nm1: ~p~n", [ClientPublic, M1]),
	{ClientPublic, M1}.