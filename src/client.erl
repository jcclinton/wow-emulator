-module(client).
-behavior(gen_server).

-record(state, {
	socket,
	account="ALICE",
	authed=false,
	rcv_key,
	send_key
}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
%% api
-export([close/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


close(Pid) ->
	gen_server:cast(Pid, close).
	


%% public
start_link() ->
	gen_server:start_link(?MODULE, {}, []).



init({}) ->
	io:format("player CLIENT: started~n"),
	gen_server:cast(self(), connect),
	{ok, #state{}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast(connect, State = #state{account=Account}) ->
	process_flag(trap_exit, true),
	KTup = store_dummy_session_key(Account),
	{ok, Port} = application:get_env(world_port),
	{ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, true}]),
	{noreply, State#state{socket=Socket, rcv_key=KTup, send_key=KTup}};
handle_cast(close, State) ->
	catch gen_tcp:close(State#state.socket),
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, _Socket, <<_?WO, 16#1EC?W, _/binary>>}, State = #state{account=Account, socket=Socket}) when not State#state.authed ->
	io:format("CLIENT: received auth challenge~n"),
	Opcode = opcode_patterns:getNumByAtom(cmsg_challenge_accept),
	Name = list_to_binary(Account),
	Payload = <<1?L, 1?L, Name/binary, 0?B, 0?B>>,
	Length = byte_size(Payload) + 4,
	Packet = <<Length?WO, Opcode?L, Payload/binary>>,
	gen_tcp:send(Socket, Packet),
	{noreply, State#state{authed=true}};
handle_info({tcp, _Socket, <<EncryptedHeader?L, Payload/binary>>}, State = #state{socket=Socket, rcv_key=RcvKeyState}) when State#state.authed ->
		EncryptedHeaderBin = <<EncryptedHeader?L>>,
		{Header, NewRcvKeyState} = world_crypto:decrypt(EncryptedHeaderBin, RcvKeyState),
		<<_Length?WO, Opcode?W>> = Header,
		io:format("client received opcode: ~p~n", [Opcode]),
	{noreply, State#state{rcv_key=NewRcvKeyState}};
handle_info(Msg, State) ->
	io:format("CLIENT: received unexpected response: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.


%% private
store_dummy_session_key(Account) ->
	Prime = srp:getPrime(),
	Generator = srp:getGenerator(),
	Private = srp:generatePrivate(),
	Salt = srp:generatePrivate(),
	Public = srp:getClientPublic(Generator, Prime, Private),
	DerivedKey = srp:getDerivedKey(<<"dummy">>, <<"data">>, Salt),
	Verifier = srp:getVerifier(Generator, Prime, DerivedKey),
	Skey = srp:computeServerKey(Private, Public, Public, Prime, Verifier),
	Key = srp:interleaveHash(Skey),
	KeyL = srp:b_to_l_endian(Key, 320),
	io:format("inserting ~p with key ~p~n", [KeyL, Account]),
	ets:insert(connected_clients, {Account, KeyL}),
  {0, 0, binary_to_list(KeyL)}.
