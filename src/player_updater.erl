-module(player_updater).
-behavior(gen_server).

-record(state, {
	account_id,
	guid,
	timer,
	marked_indices
}).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([mark_update/2]).
-export([update/0]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").



%% public api
mark_update(Guid, Indices) when is_list(Indices) ->
	Pid = get_pid(Guid),
	gen_server:cast(Pid, {mark_update, Indices}).

update() -> ok.



%% behavior callbacks

start_link(AccountId, Guid) ->
	gen_server:start_link(?MODULE, {AccountId, Guid}, []).

init({AccountId, Guid}) ->
	io:format("char updater started for ~p~n", [Guid]),

	Key = build_pid_key(Guid),
	gproc:reg({n, l, Key}, none),

	{ok, #state{account_id=AccountId, guid=Guid, timer=none, marked_indices=[]}}.


handle_cast({mark_update, Indices}, State = #state{marked_indices=StoredIndices, timer=Timer}) ->
	NewTimer = if Timer /= none ->
			Timer;
		Timer == none ->
			{ok, Tref} = timer:send_after(?game_tick, send_update),
			Tref
	end,

	NewIndices = Indices ++ StoredIndices,
	{noreply, State#state{marked_indices=NewIndices, timer=NewTimer}};
handle_cast(Msg, State) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, State}.


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.


handle_info(send_update, State = #state{guid=Guid, marked_indices=Indices}) ->

	% if any values have been changed, do update
	if Indices /= [] ->
			Mask = lists:foldl(fun(Index, MaskAcc) ->
				update_mask:set_bit(Index, MaskAcc)
			end, update_mask:empty_player(), Indices),

			Values = char_data:get_values(Guid),
			{OpAtom, Msg} = update_data:build_update_packet(Mask, Values),
			world:send_to_all(OpAtom, Msg),
			ok;
		Indices == [] -> ok
	end,

	{noreply, State#state{marked_indices=[], timer=none}};
handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, _State) ->
	ok.



%%%%%%%%%%%
%% private

build_pid_key(Guid) ->
	{?MODULE, Guid}.

get_pid(Guid) when is_number(Guid) ->
	Key = build_pid_key(Guid),
	gproc:lookup_pid({n, l, Key}).
