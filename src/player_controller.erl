-module(player_controller).
-behavior(gen_server).

-record(state, {
	account_id,
  send_pid,
	values
							 }).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([send/1, send/2]).
-compile([export_all]).


-include("include/binary.hrl").

%send message to self
send(Msg) ->
	gen_server:cast(self(), {send_to_client, Msg}).
send(Name, Msg) ->
	Pid = world:get_pid(Name),
	gen_server:cast(Pid, {send_to_client, Msg}).



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


handle_call(_E, _From, State) ->
	{reply, ok, State}.

handle_cast({tcp_packet_rcvd, <<Opcode?L, Payload/binary>>}, S = #state{account_id=AccountId, values=Values}) ->
	%io:format("looking up opcode ~p for ~p~n", [Opcode, AccountId]),
	OpcodeAtom = opcodes:getAtomByNum(Opcode),
	{M, F} = opcodes:getCallbackByNum(OpcodeAtom),
	Args = [{payload, Payload}, {account_id, AccountId}, {controller_pid, self()}, {values, Values}],
	NewValues = try M:F(Args) of
		ok -> Values;
		Result ->
			Result
			%proplists:get_value(values, Result, Values)
		catch
			badarg -> Values
		end,
	{noreply, S#state{values=NewValues}};
handle_cast({send_to_client, Msg}, S=#state{send_pid = SendPid}) ->
	gen_fsm:send_event(SendPid, {send, Msg}),
	{noreply, S};
handle_cast(Msg, S) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, S}.

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
