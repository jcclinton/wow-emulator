-module(player_rcv).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([accept/2, rcv/2, challenge/2, rcv_challenge/2]).
-export([upgrade/0]).

-include("include/binary.hrl").
-include("include/network_defines.hrl").

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
    {ok, accept, #state{hdr_len=?RCV_HDR_LEN, parent_pid=ParentPid}}.

accept({accept, ListenSocket}, State) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	process_flag(trap_exit, true),
	%% start another acceptor
	players_sup:start_socket(),
	io:format("WORLD received accept socket: ~p~n", [AcceptSocket]),
	challenge(ok, State#state{socket=AcceptSocket}).
challenge(_, State = #state{socket=Socket}) ->
	Seed   = random:uniform(16#FFFFFFFF),
	Payload = <<Seed?L>>,
	Opcode = opcodes:get_num_by_atom(smsg_auth_challenge),
	network:send_packet(Opcode, Payload, ?SEND_HDR_LEN, _KeyState=nil, Socket, _ShouldEncrypt=false),
	rcv_challenge(ok, State).
rcv_challenge(_, State = #state{socket=Socket, parent_pid=ParentPid, hdr_len=HdrLen}) ->
	try network:receive_packet(HdrLen, _KeyState=nil, Socket, _ShouldDecrypt=false) of
		{Opcode, PayloadIn, _} ->
			{PayloadOut, AccountId, KeyState} = auth_session(PayloadIn),
			%% now authorized

			start_siblings(Socket, KeyState, AccountId, ParentPid),

			player_router:packet_received(AccountId, Opcode, PayloadOut),
			rcv(ok, State#state{key_state=KeyState, account_id=AccountId})
	catch
		Error -> {stop, Error, State}
	end.
rcv(_, State = #state{socket=Socket, hdr_len=HdrLen, key_state=KeyState, account_id=AccountId}) ->
	try network:receive_packet(HdrLen, KeyState, Socket, _ShouldDecrypt=true) of
		{Opcode, Payload, NewKeyState} ->
			%io:format("rcv: received payload ~p~n", [Rest]),
			player_router:packet_received(AccountId, Opcode, Payload),
			rcv(ok, State#state{key_state=NewKeyState})
	catch
		Error -> {stop, Error, State}
	end.

%% callbacks
handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	{next_state, StateName, State}.

terminate(_Reason, StateName, _State) ->
	io:format("WORLD RCV: closing connected realm_socket~n"),
	catch gen_tcp:close(StateName#state.socket),
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.


upgrade() -> ok.



%% private

auth_session(Rest) ->
    {_, A, _}      = cmsg_auth_session(Rest),
    Data   = smsg_auth_response(),
		%io:format("authorizing session for ~p~n", [A]),
    K      = world_crypto:encryption_key(A),
    KTup     = {0, 0, K},
    {Data, A, KTup}.

cmsg_auth_session(<<Build?L, _Unk?L, Rest/binary>>) ->
    {Account, Key} = cmsg_auth_session_extract(Rest, <<>>),
    {Build, Account, Key};
cmsg_auth_session(_) ->
    {error, bad_cmsg_auth_session}.

cmsg_auth_session_extract(<<0?B, Rest/binary>>, Account) ->
    {binary_to_list(Account), Rest};
cmsg_auth_session_extract(<<Letter?B, Rest/binary>>, Account) ->
    cmsg_auth_session_extract(Rest, <<Account/binary, Letter?B>>).

smsg_auth_response() ->
    <<16#0c?B, 0?L, 0?B, 0?L>>.


start_siblings(Socket, KeyState, AccountId, ParentPid) ->
	SendPid = start_child(player_send, [Socket, KeyState], ParentPid, worker),
	_ = start_child(player_router_sup, [AccountId, SendPid], ParentPid, supervisor),
	ok.

start_child(Name, Args, ParentPid, Type) ->
	Spec = {Name,
		{Name, start_link, Args},
		permanent, 2000, Type, [Name]},
	{ok, Pid} = supervisor:start_child(ParentPid, Spec),
	Pid.
