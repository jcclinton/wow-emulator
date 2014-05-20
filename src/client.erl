-module(client).
-behavior(gen_server).

-record(state, {realm_socket, world_socket, server_public, client_private, generator, prime, salt, keyState}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
%% api
-export([challenge/0, proof/0, realmlist/0, worldconnect/0]).

-include("include/binary.hrl").
-include("include/world_records.hrl").




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
	ClientPrivate = srp:generatePrivate(),
	{noreply, State#state{realm_socket=Socket, client_private=ClientPrivate}};
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
	gen_tcp:send(State#state.world_socket, Msg),
	A = binary_to_list(getUsername(upper)),
	Key = world_crypto:encryption_key(A),
	KTup = {0, 0, Key},
	{noreply, State#state{keyState=KTup}};
handle_info({tcp, Socket, Msg = <<EncryptedLength?WO, EncryptedOpcode?W, _Rest/binary>>}, State) when Socket == State#state.world_socket ->
	io:format("CLIENT: received encrypted world tcp response: ~p~n", [Msg]),
	{<<Length?WO, Opcode?W>>, NewKTup} = world_crypto:decrypt(<<EncryptedLength?WO, EncryptedOpcode?W>>, State#state.keyState),
	io:format("CLIENT: received encrypted message with size ~p and opcode: ~p~n", [Length, Opcode]),
	{noreply, State#state{keyState=NewKTup}};
handle_info({tcp, Socket, Msg}, State) when Socket == State#state.world_socket ->
	io:format("CLIENT: received unexpected world tcp response: ~p~n", [Msg]),
	{noreply, State};
handle_info({tcp, _Socket, <<0?B, Msg/binary>>}, State) ->
	io:format("CLIENT: received challenge response~n"),
	<<_Err?B,
		_Unk2?B,
		ServerPublic_raw?QQ,
		_GLen?B,
		Generator_raw?B,
		_NLen?B,
		Prime_raw?QQ,
		Salt_raw?QQ,
		_Unk3?QH,
		_Unk4?B>> = Msg,

	%gen_server:cast(self(), send_proof),
	ServerPublic = <<ServerPublic_raw?QQ>>,
	Generator = <<Generator_raw?B>>,
	Prime = <<Prime_raw?QQ>>,
	Salt = <<Salt_raw?QQ>>,
	proof(),
	{noreply, State#state{server_public=ServerPublic,generator=Generator,prime=Prime,salt=Salt}};
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


%%
%%
%%
%%private
getUsername(upper) ->
	U = getUsername(),
	srp:normalize(U).

getUsername() ->
	<<"alice">>.

getPassword() ->
	<<"password123">>.


buildChallengeMessage(I) ->
	ILen = erlang:byte_size(I),
	S = 4 + 4 + 4 + 3 + 2 + 4 + 4 + 4 + 1 + ILen,
	[_Cmd = <<0?B>>,
	_Err = <<1?B>>,
	_Size = <<S?W>>,
	_GameName = [<<"WoW">>, <<$\0>>],
	_V1 = <<2?B>>,
	_V2 = <<4?B>>,
	_V3 = <<3?B>>,
	_Build = <<12345?W>>,
	_Platform = <<"68xx">>,
	_Os = <<"osxx">>,
	_Country = <<"usoa">>,
	_TzBias = <<60?L>>,
	_Ip = <<0?L>>,
	_ILen = <<ILen?B>>,
	I].


buildProofMessage(State) ->
	io:format("begining to build proof msg~n"),
	G = State#state.generator,
	P = State#state.prime,
	ClientPrivate = State#state.client_private,
	ServerPublic = State#state.server_public,
	Salt= State#state.salt,
	ClientPublic = getClientPublic(G, P, ClientPrivate),
	Skey = computeClientKey(ClientPublic, ServerPublic, G, P, Salt, ClientPrivate),
	Key = srp:interleaveHash(Skey),
	M1 = getM(ClientPublic, ServerPublic, Key, P, G, Salt),

	%ClientPublic = <<16#0f6621dd4a39e4df6e9b2d07e8169eb0d33c917276bdbb1eeefc61f20f809649:256>>,
	%M1 = <<16#c098171e12b60dc72d64eaa63614e5dff07ce1cf:160>>,
	%io:format("client public size: ~p~n", [erlang:byte_size(ClientPublic)]),
	%io:format("m1 size: ~p~n", [erlang:byte_size(M1)]),
	ClientPublicL = srp:b_to_l_endian(ClientPublic, 256),
	M1L = srp:b_to_l_endian(M1, 160),
	[_Cmd = <<1?B>>,
	 ClientPublicL,
	 M1L,
	 _Crc_hash = <<0?SH>>,
	 _Num_keys = <<0?B>>,
	 _Unk = <<0?B>>
	].

buildRealmlistMessage() ->
	[_Cmd = <<16?B>>,
	 _unk = <<"abcde">>
	].

build_world_challenge_response(_Seed) ->
	Build = <<1?L>>,
	Unk = <<1?L>>,
	AccountName = getUsername(upper),
	Null = <<0?B>>,
	Rest = [AccountName, Null],
	Msg = [Build, Unk, Rest],
	Opcode = 493,
	Size = size(Build) + size(Unk) + size(AccountName) + size(Null) + 4,
	Header = [<<Size?WO>>, <<Opcode?L>>],
	[Header, Msg].

getM(ClientPublic, ServerPublic, Key, Prime, Generator, Salt) ->
	I = getUsername(),
	srp:getM1(Prime, Generator, I, Salt, ClientPublic, ServerPublic, Key).

computeClientKey(ClientPublic, ServerPublic, G, P, Salt, ClientPrivate) ->
	DerivedKey = getDerivedKey(Salt),
	srp:computeClientKey(ClientPrivate, ServerPublic, ClientPublic, G, P, DerivedKey).

getClientPublic(G, P, ClientPriv) ->
	srp:getClientPublic(G, P, ClientPriv).

getDerivedKey(Salt) ->
	U = getUsername(),
	Pw = getPassword(),
	srp:getDerivedKey(U, Pw, Salt).

hash(L) ->
	crypto:hash(sha, L).
