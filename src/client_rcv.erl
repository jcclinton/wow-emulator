-module(client_rcv).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([connect/2, world_challenge/2, rcv/2]).
-export([upgrade/0]).
-export([get_pid/1]).

-include("include/binary.hrl").

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
	{ok, connect, #state{hdr_len=4, parent_pid=ParentPid, account_id=Account}}.

connect(ok, State=#state{account_id=Account}) ->
	process_flag(trap_exit, true),
	KTup = store_dummy_session_key(Account),
	{ok, Port} = application:get_env(world_port),
	{ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, false}]),
	world_challenge(ok, State#state{socket=Socket, rcv_key=KTup}).
world_challenge(_, State = #state{socket=Socket, hdr_len=HdrLen, account_id=Account, rcv_key=KeyState, parent_pid=ParentPid}) ->

	%io:format("CLIENT: received auth challenge~n"),
	{ok, <<Length?WO>>} = gen_tcp:recv(Socket, 2),
	{ok, <<16#1EC?W, _/binary>>} = gen_tcp:recv(Socket, Length),
	Opcode = opcode_patterns:getNumByAtom(cmsg_auth_session),
	Name = list_to_binary(Account),
	Payload = <<1?L, 1?L, Name/binary, 0?B, 0?B>>,
	PayloadLength = byte_size(Payload) + HdrLen,
	Packet = <<PayloadLength?WO, Opcode?L, Payload/binary>>,
	gen_tcp:send(Socket, Packet),

	%start siblings
	SendName = client_send,
	SendSpec = {SendName,
		{SendName, start_link, [Socket, KeyState]},
		transient, 5000, worker, [SendName]},
	{ok, SendPid} = supervisor:start_child(ParentPid, SendSpec),

	ControllerName = client_controller,
	ControlSpec = {ControllerName,
		{ControllerName, start_link, [Account, SendPid]},
		transient, 5000, worker, [ControllerName]},
	{ok, _} = supervisor:start_child(ParentPid, ControlSpec),

	rcv(ok, State).
rcv(ok, State = #state{socket=Socket, hdr_len=HdrLen, rcv_key=KeyState, account_id=AccountId}) ->
	%io:format("waiting for client header~n"),
	case gen_tcp:recv(Socket, HdrLen) of
		{ok, EncryptedHeader} ->
			%io:format("received encrypted header: ~p~n", [EncryptedHeader]),
			{Header, NewKeyState} = world_crypto:decrypt(EncryptedHeader, KeyState),
			%io:format("decrypted header: ~p~n", [Header]),

			<<LengthRaw?WO, Opcode?W>> = Header,
			Length = LengthRaw - 2,
			io:format("client rcv: received opcode ~p with length ~p on account ~p~n", [Opcode, Length, AccountId]),
			Rest = if Length > 0 ->
					{ok, Data} = gen_tcp:recv(Socket, Length),
					Data;
				true -> <<"">>
			end,
			%io:format("rcv: received payload ~p~n", [Rest]),
			Payload = <<Opcode?LB, Rest/binary>>,
			Msg = {tcp_packet_rcvd, Payload},
			%% sends to a process that handles the operation for this opcode, probaly a 'user' process
			Pid = get_pid(AccountId),
			gen_server:cast(Pid, Msg),
			rcv(ok, State#state{rcv_key=NewKeyState});
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
	io:format("inserting ~p with key ~p~n", [KeyL, Account]),
	char_data:store_connected_client(Account, KeyL),
  {0, 0, binary_to_list(KeyL)}.


get_pid(Name) -> world:get_pid(Name ++ "client").
