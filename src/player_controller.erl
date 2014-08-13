-module(player_controller).
-behavior(gen_server).

-record(state, {
	account_id,
  parent_pid,
	send_pid
}).


-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([packet_received/3, login_char/2, logout_char/2, send/3, send/4]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").


%% public api

packet_received(AccountId, Opcode, Payload) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {packet_rcvd, Opcode, Payload}).

% normal send enqueues outgoing packets and sends them all at once
% packets can be sent straight through by passing in the fast atom as the type
send(AccountId, OpAtom, Payload) ->
	% enqueue is the default
	send(AccountId, OpAtom, Payload, enqueue).
send(AccountId, OpAtom, Payload, Type) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {send_to_client, OpAtom, Payload, Type}).


login_char(AccountId, Guid) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {login_char, Guid}).

logout_char(AccountId, Guid) ->
	Pid = get_pid(AccountId),
	% this may error if client is shutdown suddenly
	% because the player_character process waits several seconds to shutdown
	% but it is not important at that point
	% so just ignore the error
	catch gen_server:cast(Pid, {logout_char, Guid}).




%% behavior callbacks

start_link(AccountId, SendPid, ParentPid) ->
	gen_server:start_link(?MODULE, {AccountId, SendPid, ParentPid}, []).

init({AccountId, SendPid, ParentPid}) ->
	io:format("controller SERVER: started~n"),
	Key = build_pid_key(AccountId),
	gproc:reg({n, l, Key}, none),
	{ok, #state{account_id=AccountId, send_pid=SendPid, parent_pid=ParentPid}}.


handle_cast({packet_rcvd, Opcode, Payload}, State = #state{account_id=AccountId}) ->
	%io:format("looking up opcode ~p for ~p~n", [Opcode, AccountId]),
	OpAtom = opcodes:get_atom_by_num(Opcode),

	case opcodes:get_callback_by_num(OpAtom) of
		none ->
			io:format("unknown callback: ~p~n", [OpAtom]),
			ok;
		Callback ->
			case Callback#callback.type of
				account ->
					Args = [{account_id, AccountId}, {payload, Payload}, {op_atom, OpAtom}],
					player_workers_sup:start_worker({Callback, Args}, AccountId);
				character ->
					player_character:handle_packet(AccountId, OpAtom, Callback, Payload)
			end
	end,
	{noreply, State};
handle_cast({send_to_client, OpAtom, Payload, Type}, State=#state{send_pid = SendPid}) ->
	Opcode = opcodes:get_num_by_atom(OpAtom),
	%io:format("sending ~p to client~n", [OpAtom]),
	player_send:send_msg(SendPid, Opcode, Payload, Type),
	{noreply, State};
handle_cast({login_char, Guid}, State=#state{account_id = AccountId, parent_pid = ParentPid}) ->

	Name = player_character,
	Spec = {Name,
		{Name, start_link, [AccountId, Guid]},
		transient, 1000, worker, [Name]},
	{ok, _Pid} = supervisor:start_child(ParentPid, Spec),

	{noreply, State};
handle_cast({logout_char, _Guid}, State=#state{parent_pid = ParentPid}) ->
	Name = player_character,
	supervisor:terminate_child(ParentPid, Name),
	supervisor:delete_child(ParentPid, Name),
	{noreply, State};
handle_cast(Msg, State) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, State}.


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, _State) ->
	io:format("WORLD: shutting down controller~n"),
	ok.




%%%%%%%%%%%%%%%%
%% private


build_pid_key(AccountId) ->
	AccountId ++ "controller".

get_pid(AccountId) ->
	Key = build_pid_key(AccountId),
	% this may error if client is shutdown suddenly
	catch gproc:lookup_pid({n, l, Key}).

