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
	io:format("received accept socket~n"),
	challenge(ok, State#state{accept_socket=AcceptSocket}).
challenge(_, State = #state{accept_socket=Socket}) ->
	Msg = buildAuthChallenge(),
	io:format("sending auth challenge~n"),
	gen_tcp:send(Socket, Msg),
	rcv_challenge(ok, State).
rcv_challenge(_, State = #state{accept_socket=Socket, pair_pid=PairPid}) ->
	%{ok, Packet} = gen_tcp:recv(Socket, 0),

	%<<Length?WO, 493?L, Msg/binary>> = Packet,
	%io:format("received world challenge with length ~p and msg: ~p~n", [Length, Msg]),

	{ok, Packet} = gen_tcp:recv(Socket, 2),
	<<Length?WO>> = Packet,
	io:format("received world challenge with length ~p~n", [Length]),
	{ok, PacketData} = gen_tcp:recv(Socket, Length),
	io:format("received world challenge data~n"),
	<<493?L, Msg/binary>> = PacketData,

	{_ResponseName, ResponseData, _AccountId, KState} = auth_session(Msg),
	ResponseOpCode = 494,
	ok = gen_server:call(PairPid, {tcp_accept_socket, Socket, KState}),
	gen_server:cast(PairPid, {tcp_accept_challenge, <<ResponseOpCode?W, ResponseData/binary>>}),
	rcv(ok, State#state{key_state=KState}).
rcv(_, State = #state{accept_socket=Socket, hdr_len=HdrLen, pair_pid=PairPid, key_state=KeyState}) ->
	%% TODO handle error case
	io:format("waiting for client header of length ~p...~n", [HdrLen]),
	Resp = gen_tcp:recv(Socket, HdrLen),
	{ok, Packet} = Resp,
	io:format("received encrypted header: ~p~n", [Resp]),
	%io:format("received tcp data with socket: ~p and hdrlen: ~p and with resp: ~p~n", [Socket, HdrLen, Resp]),
	<<EncryptedLength?WO, EncryptedOpcode?W>> = Packet,
	{EData, NewKState} = world_crypto:decrypt(<<EncryptedLength?WO, EncryptedOpcode?W>>, KeyState),
	io:format("decrypted data: ~p~n", [EData]),
	<<Length?WO, Opcode?W>> = EData,
	io:format("rcv: received opcode ~p with length ~p~n", [Opcode, Length]),
	Result = gen_tcp:recv(Socket, Length),
	case Result of
		{error, closed} -> ok;
		{ok, Rest} ->
			io:format("rcv: received payload ~p~n", [Rest]),
			Payload = <<Opcode?W, Rest/binary>>,
			Msg = {tcp_packet_rcvd, Payload},
			%% sends to a process that handles the operation for this opcode, probaly a 'user' process
			gen_server:cast(PairPid, Msg)
		end,
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
		io:format("authorizing session for ~p~n", [A]),
    K      = world_crypto:encryption_key(A),
    KTup     = {0, 0, K},
		AccountId = srp:getUsername(),
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
