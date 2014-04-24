-module(sockserv_rcv).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, locked/2, open/2]).

-include("include/binary.hrl").

-record(state, {accept_socket,
				hdr_len,
				key_state,
				pair_pid,
				parent_pid,
				sess_key
				}).

start_link(ListenSocket, SKey, PairPid, ParentSupPid) ->
    gen_fsm:start_link({ListenSocket, SKey, PairPid, ParentSupPid}, []).

init({ListenSocket, SKey, PairPid, ParentSupPid}) ->
    gen_fsm:send_event(self(), {accept, ListenSocket}).
	KData = {0, 0, SKey},
    {ok, accept, #state{hdr_len=4, pair_pid=PairPid, sess_key=SKey, key_state=KData, parent_pid=ParentSupPid}}.

accept({accept, ListenSocket}, State = #state{parent_pid=ParentSupPid, key_state=SKey}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	%% TODO store accept socket in ets
	%% restore it upon process crash
	ok = gen_server:call(PairPid, {tcp_accept_socket, AcceptSocket}),
	rcv(ok, State#state{accept_socket=AcceptSocket};
rcv(ok, State = #state{accept_socket=Socket, hdr_len=HdrLen, pair_pid=PairPid, key_state}) ->
	%% TODO handle error case
	{ok, Packet} = gen_tcp:recv(Socket, HdrLen),
	<<EncryptedLength?WO, EncryptedOpcode?W>> = Packet,
	{<<Length?WO, Opcode?W>>, NewKState} = world_crypto:decrypt(<<EncryptedLength?WO, EncryptedOpcode?W>>, KeyState),
	{ok, Rest} = gen_tcp:recv(Socket, Length),
	Payload = <<Opcode?W, Rest/binary>>,
	Msg = {tcp_packet_rcvd, Payload},
	%% sends to a process that handles the operation for this opcode, probaly a 'user' process
	gen_server:cast(PairPid, Msg),
	rcv(ok, State#state{key_state=NewKState};

