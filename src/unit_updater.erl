-module(unit_updater).
-behavior(gen_fsm).

-record(state, {
	guid,
	type,
	marked_indices
}).


-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([idle/2, accumulate/2]).
-export([update/0]).
-export([mark_update/2]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").



%% public api
mark_update(Guid, Indices) when is_list(Indices) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_fsm:send_event(Pid, {mark_update, Indices}).

update() -> ok.



%% behavior callbacks

start_link(Guid, Type) ->
	gen_fsm:start_link(?MODULE, {Guid, Type}, []).

init({Guid, Type}) ->
	io:format("char updater started for ~p~n", [Guid]),

	util:reg_proc(?MODULE, Guid),

	{ok, idle, #state{guid=Guid, marked_indices=[], type=Type}}.



idle({mark_update, Indices}, State) ->
	timer:apply_after(?game_tick, gen_fsm, send_event, [self(), send]),
	%io:format("starting timer~n"),
	{next_state, accumulate, State#state{marked_indices=Indices}}.

accumulate({mark_update, Indices}, State = #state{marked_indices=StoredIndices}) ->
	%io:format("accing~n"),
	NewIndices = Indices ++ StoredIndices,
	{next_state, accumulate, State#state{marked_indices=NewIndices}};
accumulate(send, State = #state{marked_indices=Indices, type=Type, guid=Guid}) ->
	%io:format("sending~n"),

	Mask = lists:foldl(fun(Index, MaskAcc) ->
		update_mask:set_bit(Index, MaskAcc)
	end, update_mask:empty(Type), Indices),

	Values = char_data:get_values(Guid),
	{OpAtom, Msg} = update_data:build_update_packet(Mask, Values),
	world:send_to_all(OpAtom, Msg),

	{next_state, idle, State#state{marked_indices=[]}}.



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
