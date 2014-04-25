-module(sockserv_rcv).
-behaviour(gen_fsm).

-export([start_link/3]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([accept/2, rcv/2, challenge/2, rcv_challenge/2]).

-include("include/binary.hrl").

-record(state, {accept_socket,
				hdr_len,
				key_state,
				pair_pid,
				sess_key
				}).

start_link(ListenSocket, SKey, PairPid) ->
    gen_fsm:start_link(?MODULE, {ListenSocket, SKey, PairPid}, []).

init({ListenSocket, SKey, PairPid}) ->
	io:format("starting rcv~n"),
    gen_fsm:send_event(self(), {accept, ListenSocket}),
	KData = {0, 0, SKey},
    {ok, accept, #state{hdr_len=4, pair_pid=PairPid, sess_key=SKey, key_state=KData}}.

accept({accept, ListenSocket}, State = #state{pair_pid=PairPid}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	%% TODO store accept socket in ets
	%% restore it upon process crash
	ok = gen_server:call(PairPid, {tcp_accept_socket, AcceptSocket}),
	challenge(ok, State#state{accept_socket=AcceptSocket}).
challenge(_, State = #state{accept_socket=Socket}) ->
	Msg = buildAuthChallenge(),
	gen_tcp:send(Socket, Msg),
	rcv_challenge(ok, State).
rcv_challenge(_, State = #state{accept_socket=Socket, pair_pid=PairPid}) ->
	{ok, Packet} = gen_tcp:recv(Socket, 0),
	<<_Length?WO, 493?L, Msg/binary>> = Packet,
	gen_server:cast(PairPid, {tcp_accept_challenge, Msg}),
	rcv(ok, State).
rcv(_, State = #state{accept_socket=Socket, hdr_len=HdrLen, pair_pid=PairPid, key_state=KeyState}) ->
	%% TODO handle error case
	Resp = gen_tcp:recv(Socket, HdrLen),
	%io:format("received tcp data with socket: ~p and hdrlen: ~p and with resp: ~p~n", [Socket, HdrLen, Resp]),
	{ok, Packet} = Resp,
	<<EncryptedLength?WO, EncryptedOpcode?W>> = Packet,
	{<<Length?WO, Opcode?W>>, NewKState} = world_crypto:decrypt(<<EncryptedLength?WO, EncryptedOpcode?W>>, KeyState),
	{ok, Rest} = gen_tcp:recv(Socket, Length),
	Payload = <<Opcode?W, Rest/binary>>,
	Msg = {tcp_packet_rcvd, Payload},
	%% sends to a process that handles the operation for this opcode, probaly a 'user' process
	gen_server:cast(PairPid, Msg),
	rcv(ok, State#state{key_state=NewKState}).

%% callbacks
handle_info(_Info, State, Data) ->
	{next_state, State, Data}.

handle_event(_Event, State, Data) ->
	{next_state, State, Data}.

handle_sync_event(_Event, _From, State, Data) ->
	{next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
	ok.

code_change(_OldVsn, State, Data, _Extra) ->
	{ok, State, Data}.



%% private
buildAuthChallenge() ->
	Opcode = 492,
	Seed   = random:uniform(16#FFFFFFFF),
	Msg = [
	 O = <<Opcode?W>>,
	 S = <<Seed?L>>
	],
	Length = size(O) + size(S),
	[<<Length?WO>>, Msg].
