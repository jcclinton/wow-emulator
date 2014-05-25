-module(world_socket_rcv).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([accept/2, rcv/2, challenge/2, rcv_challenge/2]).

-include("include/binary.hrl").

-record(state, {socket,
				hdr_len,
				pair_pid,
				sess_key,
				key_state
				}).

start_link(ListenSocket, PairPid) ->
    gen_fsm:start_link(?MODULE, {ListenSocket, PairPid}, []).

init({ListenSocket, PairPid}) ->
	io:format("starting rcv~n"),
    gen_fsm:send_event(self(), {accept, ListenSocket}),
    {ok, accept, #state{hdr_len=6, pair_pid=PairPid}}.

accept({accept, ListenSocket}, State = #state{}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	%% start another acceptor
	world_server_sup:start_socket(),
	%io:format("received accept socket~n"),
	challenge(ok, State#state{socket=AcceptSocket}).
challenge(_, State = #state{socket=Socket}) ->
	Msg = buildAuthChallenge(),
	%io:format("sending auth challenge~n"),
	gen_tcp:send(Socket, Msg),
	rcv_challenge(ok, State).
rcv_challenge(_, State = #state{socket=Socket, pair_pid=PairPid}) ->
	%{ok, Packet} = gen_tcp:recv(Socket, 0),

	%<<Length?WO, 493?L, Msg/binary>> = Packet,
	%io:format("received world challenge with length ~p and msg: ~p~n", [Length, Msg]),

	{ok, Packet} = gen_tcp:recv(Socket, 2),
	<<Length?WO>> = Packet,
	%io:format("received world challenge with length ~p~n", [Length]),
	{ok, PacketData} = gen_tcp:recv(Socket, Length),
	%io:format("received world challenge data~n"),
	<<493?L, Msg/binary>> = PacketData,

	{_ResponseName, ResponseData, AccountId, KeyState} = auth_session(Msg),
	ok = gen_server:call(PairPid, {tcp_accept_socket, Socket, AccountId , KeyState}),
	ResponseOpCode = 494,
	gen_server:cast(PairPid, {tcp_accept_challenge, <<ResponseOpCode?W, ResponseData/binary>>}),
	rcv(ok, State#state{key_state=KeyState}).
rcv(_, State = #state{socket=Socket, hdr_len=HdrLen, pair_pid=PairPid, key_state=KeyState}) ->
	%% TODO handle error case
	%io:format("waiting for client header~n"),
	Resp = gen_tcp:recv(Socket, HdrLen),
	NewKeyState = case Resp of
		{ok, EncryptedHeader} ->
			%io:format("received encrypted header: ~p~n", [EncryptedHeader]),
			{Header, KeyState2} = world_crypto:decrypt(EncryptedHeader, KeyState),
			%io:format("decrypted header: ~p~n", [Header]),

			<<LengthRaw?WO, Opcode?L>> = Header,
			Length = LengthRaw - 4,
			io:format("rcv: received opcode ~p with length ~p~n", [Opcode, Length]),
			Rest = if Length > 0 ->
					{ok, Data} = gen_tcp:recv(Socket, Length),
					Data;
				true -> <<"">>
			end,
			%io:format("rcv: received payload ~p~n", [Rest]),
			Payload = <<Opcode?LB, Rest/binary>>,
			Msg = {tcp_packet_rcvd, Payload},
			%% sends to a process that handles the operation for this opcode, probaly a 'user' process
			gen_server:cast(PairPid, Msg),
			KeyState2;
		_ -> KeyState
		end,
	rcv(ok, State#state{key_state=NewKeyState}).

%% callbacks
handle_info(_Info, State, Data) ->
	{next_state, State, Data}.

handle_event(_Event, State, Data) ->
	{next_state, State, Data}.

handle_sync_event(_Event, _From, State, Data) ->
	{next_state, State, Data}.

terminate(_Reason, State, _Data) ->
	io:format("WORLD RCV: closing connected realm_socket~n"),
	catch gen_tcp:close(State#state.socket),
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
    {smsg_auth_response, Data, A, KTup}.

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
    <<16#0c?B, 0?L, 0?B, 0?L>>.
