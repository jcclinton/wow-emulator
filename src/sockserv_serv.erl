-module(sockserv_serv).
-behavior(gen_server).

-record(state, {name, % players name
								socket, % the current socket
								identity,
								apub,
								m1
							 }).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("include/binary.hrl").


start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
	io:format("SERVER: started~n"),
	gen_server:cast(self(), accept),
	{ok, #state{socket=Socket}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast(accept, S = #state{socket=ListenSocket}) ->
	%io:format("LSock: ~p~n", [ListenSocket]),
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	%io:format("ASock: ~p~n", [AcceptSocket]),
	%io:format("SERVER: socket accepted~n"),
	%sockserv_sup:start_socket(),
	{noreply, S#state{socket=AcceptSocket}};
handle_cast(send_challenge, State = #state{socket=Socket}) ->
	Msg = build_challenge_response(),
	gen_tcp:send(Socket, Msg),
	{noreply, State};
handle_cast(send_proof, State = #state{socket=Socket, m1=M1, apub=Apub}) ->
	Msg = build_proof_response(M1, Apub),
	gen_tcp:send(Socket, Msg),
	{noreply, State}.

handle_info({tcp, _Socket, <<0?B, Msg/binary>>}, State) ->
	ok = inet:setopts(State#state.socket, [{active, once}]),
	I = try extract_username(Msg) of
		_ -> extract_username(Msg)
	catch
		throw:zero_ilen -> gen_tcp:send(State#state.socket, <<100>>),
											 io:format("zero_ilen!~n"),
											 <<"">>;
		throw:bad_i_size -> gen_tcp:send(State#state.socket, <<200>>),
											 io:format("bad_i_size!~n"),
												<<"">>;
				_ -> <<"">>
	end,
	io:format("SERVER: received challenge~n"),
	%io:format("SERVER: received: ~p~n", [Msg]),
	%io:format("SERVER: received name: ~p~n", [I]),
	gen_server:cast(self(), send_challenge),
	{noreply, State#state{identity=I}};
handle_info({tcp, _Socket, <<1?B, Msg/binary>>}, State) ->
	ok = inet:setopts(State#state.socket, [{active, once}]),
	io:format("SERVER: received proof~n"),
	{Apub, M1} = extract_proof(Msg),
	gen_server:cast(self(), send_proof),
	{noreply, State#state{apub=Apub, m1=M1}};
handle_info(_Msg, State) ->
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(normal, _State) ->
	ok;
terminate(_Reason, _State) ->
	ok.


%% private
build_challenge_response() ->
	B = logon_lib:getServerPublic(),
	G = logon_lib:getGenerator(),
	N = logon_lib:getPrime(),
	S = logon_lib:getSalt(),
	GLen = erlang:byte_size(G),
	NLen = erlang:byte_size(N),
	%io:format("bsize: ~p~n", [erlang:byte_size(B)]),
	%io:format("gsize: ~p~n", [erlang:byte_size(G)]),
	%io:format("nsize: ~p~n", [erlang:byte_size(N)]),
	%io:format("ssize: ~p~n", [erlang:byte_size(S)]),
	Msg = [_Cmd = <<0?B>>,
					_Err = <<0?B>>,
					_Unk2 = <<0?B>>,
					B,
					<<GLen?B>>,
					G,
					<<NLen?B>>,
					N,
					S,
					_Unk3 = <<0?W>>,
					_Unk4 = <<0?W>>
				],
	Msg.

build_proof_response(M1_client, Apub) ->
	Bpub = logon_lib:getServerPublic(),
	Skey = logon_lib:computeServerKey(Apub),
	M1_server = logon_lib:getM(Apub, Bpub, Skey),
	M1_client = M1_server,
	K = logon_lib:hash([Skey]),
	M2 = logon_lib:hash([Apub, M1_server, K]),
	Msg = [_Cmd = <<1?B>>,
				 _Err = <<0?B>>,
				 M2,
				 _Flags = <<0?B>>],
	Msg.

extract_username(Msg) ->
		<<_Err?B,
		_Size?W,
		_GameName?L,
		_V1?B,
		_V2?B,
		_V3?B,
		_Build?W,
		_Platform?L,
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
		<<Iout:Size, Rest/binary>> = I,
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
		Iout.

extract_proof(Msg) ->
	<<Apub_raw:1024,
		M1_raw?SH,
		_Crc_hash?SH,
		_Num_keys?B,
		_Unk?B>> = Msg,
	Apub = <<Apub_raw:1024>>,
	M1 = <<M1_raw?SH>>,
	{Apub, M1}.
