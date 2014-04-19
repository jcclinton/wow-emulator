-module(client).
-behavior(gen_server).

-record(state, {realm_socket, world_socket, bpub, generator, prime, salt}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
%% api
-export([challenge/0, proof/0, realmlist/0, worldconnect/0]).

-include("include/binary.hrl").

%%
%%
%%
%%private
getUsername() -> <<"jcclinton24">>.
getPassword() -> <<"abc123">>.

buildChallengeMessage(I) ->
	ILen = erlang:byte_size(I),
	S = 4 + 3 + 2 + 4 + 4 + 4 + 1 + ILen,
	[_Cmd = <<0?B>>,
	_Err = <<1?B>>,
	_Size = <<S?W>>,
	_GameName = [<<"WoW">>, <<$\0>>],
	_V1 = <<2?B>>,
	_V2 = <<4?B>>,
	_V3 = <<3?B>>,
	_Build = <<12345?W>>,
	_Platform = <<"68xx">>,
	_TzBias = <<60?L>>,
	_Ip = <<0?L>>,
	_ILen = <<ILen?B>>,
	I].


buildProofMessage(State) ->
	io:format("begining to build proof msg~n"),
	G = State#state.generator,
	P = State#state.prime,
	Bpub = State#state.bpub,
	Salt= State#state.salt,
	Apub = getClientPublic(G, P),
	Skey = computeClientKey(Apub, Bpub, G, P, Salt),
	M1 = getM(Apub, Bpub, Skey, P, G, Salt),
	%io:format("apub size: ~p~n", [erlang:byte_size(Apub)]),
	%io:format("m1 size: ~p~n", [erlang:byte_size(M1)]),
	[_Cmd = <<1?B>>,
	 Apub,
	 M1,
	 _Crc_hash = <<0?SH>>,
	 _Num_keys = <<0?B>>,
	 _Unk = <<0?B>>
	].

buildRealmlistMessage() ->
	[_Cmd = <<16?B>>,
	 _unk = <<"abcde">>
	].

build_world_challenge_response(_Seed) ->
	Msg = [],
	Size = size(Msg),
	[<<Size?WO>>, Msg].

getM(Apub, Bpub, Skey, P, G, Salt) ->
	I = getUsername(),
	P1 = crypto:exor(hash(P), hash(G)),
	K = hash([Skey]),
	M = hash([P1, hash(I), Salt, Apub, Bpub, K]),
	M.

computeClientKey(Apub, Bpub, G, P, Salt) ->
	Version = getVersion(),
	U = getScrambler(Apub, Bpub),
	DerivedKey = getDerivedKey(Salt),
	ClientPrivate = getClientPrivate(),
	crypto:compute_key(srp, Bpub, {Apub, ClientPrivate}, {user, [DerivedKey, P, G, Version, U]}).

getClientPublic(G, P) ->
	Priv = getClientPrivate(),
	Version = getVersion(),
	%io:format("g: ~p~np: ~p~nversion: ~p~nPriv: ~p~n", [G, P, Version, Priv]),
	{Pub, _} = crypto:generate_key(srp, {user, [G, P, Version]}, Priv),
	%io:format("apub: ~p~n", [Pub]),
	Pub.

getScrambler(Apub, Bpub) ->
	hash([Apub, Bpub]).

getDerivedKey(Salt) ->
	U = getUsername(),
	Pw = getPassword(),
	hash([Salt, hash([U, <<$:>>, Pw])]).

hash(L) ->
	crypto:hash(sha, L).


getClientPrivate() -> <<"60975527035CF2AD1989806F0407210BC81EDC04E2762A56AFD529DD"
																"DA2D4393">>.

getVersion() -> '6a'.





%% public
start_link() ->
	gen_server:start_link({local, client}, ?MODULE, [], []).

challenge() ->
	gen_server:cast(client, challenge).
proof() ->
	gen_server:cast(client, send_proof).
realmlist() ->
	gen_server:cast(client, request_realmlist).
worldconnect() ->
	gen_server:cast(client, world_connect).


init([]) ->
	io:format("CLIENT: started~n"),
	gen_server:cast(self(), connect),
	{ok, #state{}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast(connect, State) ->
	process_flag(trap_exit, true),
	{ok, Port} = application:get_env(realm_port),
	{ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, true}]),
	%io:format("CLIENT: socket connected~n"),
	{noreply, State#state{realm_socket=Socket}};
handle_cast(challenge, State) ->
	I = getUsername(),
	Msg = buildChallengeMessage(I),
	gen_tcp:send(State#state.realm_socket, Msg),
	{noreply, State};
handle_cast(send_proof, State) ->
	Msg = buildProofMessage(State),
	gen_tcp:send(State#state.realm_socket, Msg),
	{noreply, State};
handle_cast(request_realmlist, State) ->
	Msg = buildRealmlistMessage(),
	gen_tcp:send(State#state.realm_socket, Msg),
	{noreply, State};
handle_cast(world_connect, State) ->
	{ok, Port} = application:get_env(world_port),
	{ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, true}]),
	io:format("client connecting to world server~n"),
	{noreply, State#state{world_socket=Socket}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, Socket, <<_HdrLen?WO, 492?W, Seed/binary>>}, State) when Socket == State#state.world_socket ->
	io:format("CLIENT: received world challenge response: ~p~n", [Seed]),
	Msg = build_world_challenge_response(Seed),
	{noreply, State};
handle_info({tcp, _Socket, <<0?B, Msg/binary>>}, State) ->
	io:format("CLIENT: received challenge response~n"),
	<<_Err?B,
		_Unk2?B,
		Bpub_raw:1024,
		_GLen?B,
		Generator_raw?B,
		_NLen?B,
		Prime_raw:1024,
		Salt_raw:128,
		_Unk3?W,
		_Unk4?W>> = Msg,
	%gen_server:cast(self(), send_proof),
	Bpub = <<Bpub_raw:1024>>,
	Generator = <<Generator_raw?B>>,
	Prime = <<Prime_raw:1024>>,
	Salt = <<Salt_raw:128>>,
	proof(),
	{noreply, State#state{bpub=Bpub,generator=Generator,prime=Prime,salt=Salt}};
handle_info({tcp, _Socket, <<1?B, _Msg/binary>>}, State) ->
	io:format("CLIENT: received proof response~n"),
	realmlist(),
	{noreply, State};
handle_info({tcp, _Socket, <<16?B, _Msg/binary>>}, State) ->
	io:format("CLIENT: received realmlist response~n"),
	worldconnect(),
	{noreply, State};
handle_info({tcp, _Socket, Msg}, State) ->
	io:format("CLIENT: received unexpected tcp response: ~p~n", [Msg]),
	{noreply, State};
handle_info(upgrade, State) ->
	%% loads latest code
	?MODULE:handle_info(do_upgrade, State),
	{noreply, State};
handle_info(Msg, State) ->
	io:format("CLIENT: received unexpected response: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(shutdown, State) ->
	io:format("CLIENT: closing connected realm_socket~n"),
	catch gen_tcp:close(State#state.realm_socket),
	io:format("CLIENT: closing connected world_socket~n"),
	catch gen_tcp:close(State#state.world_socket),
	ok;
terminate(_Reason, _State) ->
	ok.
