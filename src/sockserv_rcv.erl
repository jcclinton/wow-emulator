-module(sockserv_rcv).
-behaviour(gen_fsm).

-export([start_link/2]).
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

start_link(ListenSocket, PairPid) ->
    gen_fsm:start_link(?MODULE, {ListenSocket, PairPid}, []).

init({ListenSocket, PairPid}) ->
	io:format("starting rcv~n"),
    gen_fsm:send_event(self(), {accept, ListenSocket}),
    {ok, accept, #state{hdr_len=4, pair_pid=PairPid}}.

accept({accept, ListenSocket}, State = #state{}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	challenge(ok, State#state{accept_socket=AcceptSocket}).
challenge(_, State = #state{accept_socket=Socket}) ->
	Msg = buildAuthChallenge(),
	gen_tcp:send(Socket, Msg),
	rcv_challenge(ok, State).
rcv_challenge(_, State = #state{accept_socket=Socket, pair_pid=PairPid}) ->
	{ok, Packet} = gen_tcp:recv(Socket, 0),
	<<_Length?WO, 493?L, Msg/binary>> = Packet,
	{_ResponseName, ResponseData, _AccountId, KState} = auth_session(Msg),
	ResponseOpCode = 494,
	ok = gen_server:call(PairPid, {tcp_accept_socket, Socket, KState}),
	gen_server:cast(PairPid, {tcp_accept_challenge, <<ResponseOpCode?W, ResponseData/binary>>}),
	rcv(ok, State#state{key_state=KState}).
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


auth_session(Rest) ->
    {_, A, _}      = cmsg_auth_session(Rest),
    Data   = smsg_auth_response(),
    K      = world_crypto:encryption_key(A),
    KTup     = {0, 0, K},
		AccountId = logon_lib:getUsername(),
    {smsg_auth_response, Data, AccountId, KTup}.

cmsg_auth_session(<<Build?L, _Unk?L, Rest/binary>>) ->
    {Account, Key} = cmsg_auth_session_extract(Rest, ""),
    {Build, Account, Key};
cmsg_auth_session(_) ->
    {error, bad_cmsg_auth_session}.

cmsg_auth_session_extract(<<0?B, Rest/bytes>>, Account) ->
    {Account, binary_to_list(Rest)};
cmsg_auth_session_extract(<<Letter?B, Rest/binary>>, Account) ->
    cmsg_auth_session_extract(Rest, Account ++ [Letter]).

smsg_auth_response() ->
    <<12?B, 0?L, 0?B, 0?L, 1?B>>.
