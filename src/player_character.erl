-module(player_character).
-behavior(gen_server).

-record(state, {
	account_id,
	guid,
	update_timer
}).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_pid/1, send/3, handle_packet/4, update/1]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").

-define(update_timer_interval, 50).


%% public api

send(Name, OpAtom, Payload) ->
	RouterPid = player_controller:get_pid(Name),
	player_controller:send(RouterPid, OpAtom, Payload).

get_pid(AccountId) ->
	world:build_pid(AccountId, "char").


handle_packet(AccountId, OpAtom, Callback, Payload) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {packet_rcvd, OpAtom, Callback, Payload}).

update(AccountId) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, update).




%% behavior callbacks

start_link(AccountId, Guid) ->
	Pid = get_pid(AccountId),
	gen_server:start_link(Pid, ?MODULE, {AccountId, Guid}, []).

init({AccountId, Guid}) ->
	process_flag(trap_exit, true),
	io:format("char SERVER started for ~p~n", [Guid]),
	char_data:init_session(Guid),

	{ok, TRef} = timer:apply_interval(?update_timer_interval, ?MODULE, update, [AccountId]),
	{ok, #state{account_id=AccountId, guid=Guid, update_timer=TRef}}.


handle_cast(update, State = #state{guid=Guid}) ->
	Mask = char_data:get_mask(Guid),
	IsEmpty = update_mask:is_empty(Mask),
	if not IsEmpty ->
			Values = char_data:get_values(Guid),
			{OpAtom, Msg} = update_data:build_update_packet(Mask, Values),
			world:send_to_all(OpAtom, Msg),
			char_data:clear_mask(Guid),
			ok;
		true -> ok
	end,
	{noreply, State};
handle_cast({packet_rcvd, OpAtom, Callback, Payload}, State = #state{account_id=AccountId, guid=Guid}) ->
	Args = [{payload, Payload}, {account_id, AccountId}, {op_atom, OpAtom}, {guid, Guid}],
	player_workers_sup:start_worker({Callback, Args}, AccountId),
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

terminate(_Reason, #state{guid=Guid, update_timer=Timer}) ->
	char_data:close_session(Guid),
	timer:cancel(Timer),
	io:format("WORLD: shutting down char: ~p~n", [Guid]),
	ok.
