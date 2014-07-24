-module(client_rcv).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([connect/2, world_challenge/2, rcv/2]).
-export([upgrade/0]).

-include("include/binary.hrl").
-include("include/network_defines.hrl").

-record(state, {socket,
				hdr_len,
				parent_pid,
				rcv_key,
				account_id
				}).

start_link(ParentPid, Account) ->
    gen_fsm:start_link(?MODULE, {ParentPid, Account}, []).

init({ParentPid, Account}) ->
	io:format("client starting rcv~n"),
	gen_fsm:send_event(self(), ok),
	{ok, connect, #state{hdr_len=?SEND_HDR_LEN, parent_pid=ParentPid, account_id=Account}}.

connect(ok, State=#state{account_id=Account}) ->
	process_flag(trap_exit, true),
	KTup = store_dummy_session_key(Account),
	{ok, Port} = application:get_env(world_port),
	{ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, false}]),
	world_challenge(ok, State#state{socket=Socket, rcv_key=KTup}).
world_challenge(_, State = #state{socket=Socket, account_id=AccountId, rcv_key=KeyState, parent_pid=ParentPid, hdr_len=HdrLen}) ->

	try network:receive_packet(HdrLen, _KeyStateReceive=nil, Socket, _ShouldDecrypt=false) of
		{_Opcode, _Payload, _} ->
			Name = list_to_binary(AccountId),
			PayloadOut = <<1?L, 1?L, Name/binary, 0?B, 0?B>>,
			Opcode = opcodes:get_num_by_atom(cmsg_auth_session),
			network:send_packet(Opcode, PayloadOut, ?RCV_HDR_LEN, _KeyStateSend=nil, Socket, _ShouldEncrypt=false),

			start_siblings(Socket, KeyState, AccountId, ParentPid),

			rcv(ok, State)
	catch
		Error -> {stop, Error, State}
	end.

rcv(ok, State = #state{socket=Socket, hdr_len=HdrLen, rcv_key=KeyState, account_id=AccountId}) ->
	try network:receive_packet(HdrLen, KeyState, Socket, _ShouldDecrypt=true) of
		{Opcode, Payload, NewKeyState} ->
			client_controller:tcp_packet_received(AccountId, Opcode, Payload),
			rcv(ok, State#state{rcv_key=NewKeyState})
	catch
		Error -> {stop, Error, State}
	end.

%% callbacks
handle_info(_Info, State, Data) ->
	{next_state, State, Data}.

handle_event(_Event, State, Data) ->
	{next_state, State, Data}.

handle_sync_event(_Event, _From, State, Data) ->
	{next_state, State, Data}.

terminate(_Reason, State, _Data) ->
	io:format("client RCV: closing connected realm_socket~n"),
	catch gen_tcp:close(State#state.socket),
	ok.

code_change(_OldVsn, State, Data, _Extra) ->
	{ok, State, Data}.


upgrade() -> ok.



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
	char_data:store_connected_client(Account, KeyL),
  {0, 0, binary_to_list(KeyL)}.




start_siblings(Socket, KeyState, AccountId, ParentPid) ->
	SendName = client_send,
	SendSpec = {SendName,
		{SendName, start_link, [Socket, KeyState]},
		transient, 5000, worker, [SendName]},
	{ok, SendPid} = supervisor:start_child(ParentPid, SendSpec),

	ControllerName = client_controller,
	ControlSpec = {ControllerName,
		{ControllerName, start_link, [AccountId, SendPid]},
		transient, 5000, worker, [ControllerName]},
	{ok, _} = supervisor:start_child(ParentPid, ControlSpec),

	ok.
