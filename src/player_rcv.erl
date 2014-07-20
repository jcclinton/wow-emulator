-module(player_rcv).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([accept/2, rcv/2, challenge/2, rcv_challenge/2]).
-export([upgrade/0]).

-include("include/binary.hrl").

-record(state, {socket,
				hdr_len,
				pair_pid,
				parent_pid,
				sess_key,
				key_state,
				account_id
				}).

start_link(ListenSocket, ParentPid) ->
    gen_fsm:start_link(?MODULE, {ListenSocket, ParentPid}, []).

init({ListenSocket, ParentPid}) ->
		io:format("WORLD starting rcv~n"),
    gen_fsm:send_event(self(), {accept, ListenSocket}),
    {ok, accept, #state{hdr_len=6, parent_pid=ParentPid}}.

accept({accept, ListenSocket}, State) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	process_flag(trap_exit, true),
	%% start another acceptor
	players_sup:start_socket(),
	io:format("WORLD received accept socket: ~p~n", [AcceptSocket]),
	challenge(ok, State#state{socket=AcceptSocket}).
challenge(_, State = #state{socket=Socket}) ->
	Msg = buildAuthChallenge(),
	%io:format("sending auth challenge~n"),
	gen_tcp:send(Socket, Msg),
	rcv_challenge(ok, State).
rcv_challenge(_, State = #state{socket=Socket, parent_pid=ParentPid}) ->
	{ok, <<Length?WO>>} = gen_tcp:recv(Socket, 2),
	%io:format("received world challenge with length ~p~n", [Length]),
	{ok, <<Opcode?L, Msg/binary>>} = gen_tcp:recv(Socket, Length),
	%io:format("received world challenge data~n"),

	{_ResponseName, ResponseData, AccountId, KeyState} = auth_session(Msg),
	%% now authorized

	%start siblings
	SendName = player_send,
	SendSpec = {SendName,
		{SendName, start_link, [Socket, KeyState]},
		transient, 5000, worker, [SendName]},
	{ok, SendPid} = supervisor:start_child(ParentPid, SendSpec),

	ControllerName = player_controller_sup,
	ControlSpec = {ControllerName,
		{ControllerName, start_link, [AccountId, SendPid]},
		transient, 5000, worker, [ControllerName]},
	{ok, _} = supervisor:start_child(ParentPid, ControlSpec),

	Payload = <<Opcode?L, ResponseData/binary>>,
	Pid = world:get_pid(AccountId),
	gen_server:cast(Pid, {tcp_packet_rcvd, Payload}),
	rcv(ok, State#state{key_state=KeyState, account_id=AccountId}).
rcv(_, State = #state{socket=Socket, hdr_len=HdrLen, key_state=KeyState, account_id=AccountId}) ->
	%% TODO handle error case
	%io:format("waiting for client header~n"),
	case gen_tcp:recv(Socket, HdrLen) of
		{ok, EncryptedHeader} ->
			%io:format("received encrypted header: ~p~n", [EncryptedHeader]),
			{<<LengthRaw?WO, Opcode?L>>, NewKeyState} = world_crypto:decrypt(EncryptedHeader, KeyState),

			Length = LengthRaw - 4,
			%io:format("player rcv: received opcode ~p with length ~p on account ~p~n", [Opcode, Length, AccountId]),
			Rest = if Length > 0 ->
					{ok, Data} = gen_tcp:recv(Socket, Length),
					Data;
				true -> <<"">>
			end,
			%io:format("rcv: received payload ~p~n", [Rest]),
			Msg = {tcp_packet_rcvd, <<Opcode?L, Rest/binary>>},
			%% sends to a process that handles the operation for this opcode, probaly a 'user' process
			Pid = world:get_pid(AccountId),
			gen_server:cast(Pid, Msg),
			rcv(ok, State#state{key_state=NewKeyState});
		_ -> {stop, socket_closed, State}
		end.

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


upgrade() -> ok.



%% private
buildAuthChallenge() ->
	Opcode = opcode_patterns:getNumByAtom(smsg_auth_challenge),
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
		%io:format("authorizing session for ~p~n", [A]),
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
