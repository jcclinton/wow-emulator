-module(player_controller).
-behavior(gen_server).

-record(state, {
	account_id,
  send_pid,
	values
}).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([send/2, send/3, tcp_packet_received/3]).


-include("include/binary.hrl").


%% public api

%send message to self
send(OpAtom, Payload) ->
	send_internal(self(), OpAtom, Payload).
send(Name, OpAtom, Payload) ->
	Pid = world:get_pid(Name),
	send_internal(Pid, OpAtom, Payload).

send_internal(Pid, OpAtom, Payload) ->
	gen_server:cast(Pid, {send_to_client, OpAtom, Payload}).


tcp_packet_received(AccountId, Opcode, Payload) ->
	Msg = {tcp_packet_rcvd, Opcode, Payload},
	%% sends to a process that handles the operation for this opcode, probaly a 'user' process
	Pid = world:get_pid(AccountId),
	gen_server:cast(Pid, Msg).




%% behavior callbacks

start_link(AccountId, SendPid) ->
	Pid = world:get_pid(AccountId),
	gen_server:start_link(Pid, ?MODULE, {AccountId, SendPid}, []).

init({AccountId, SendPid}) ->
	io:format("controller SERVER: started~n"),
	process_flag(trap_exit, true),

	%TODO this is char data, not player data, should be put somewhere else
	TotalCount = update_fields:fields('PLAYER_END'),
	Values = binary:copy(<<0?L>>, TotalCount),
	{ok, #state{values=Values, send_pid=SendPid, account_id=AccountId}}.


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast({tcp_packet_rcvd, Opcode, Payload}, State = #state{account_id=AccountId, values=Values}) ->
	%io:format("looking up opcode ~p for ~p~n", [Opcode, AccountId]),
	OpAtom = opcodes:get_atom_by_num(Opcode),
	{M, F} = opcodes:get_callback_by_num(OpAtom),
	Args = [{payload, Payload}, {account_id, AccountId}, {controller_pid, self()}, {values, Values}],
	NewValues = try M:F(Args) of
		ok -> Values;
		Result ->
			Result
			%proplists:get_value(values, Result, Values)
		catch
			badarg -> Values
		end,
	{noreply, State#state{values=NewValues}};
handle_cast({send_to_client, OpAtom, Payload}, State=#state{send_pid = SendPid}) ->
	Opcode = opcodes:get_num_by_atom(OpAtom),
	player_send:send_msg(SendPid, Opcode, Payload),
	{noreply, State};
handle_cast(Msg, State) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, State}.

handle_info(upgrade, State) ->
	%% loads latest code
	?MODULE:handle_info(do_upgrade, State),
	{noreply, State};
handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, _State) ->
	io:format("WORLD: shutting down controller~n"),
	ok.
