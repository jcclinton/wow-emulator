-module(player_character).
-behavior(gen_server).

-record(state, {
	account_id,
	guid,
	update_timer,
	last_swing,
	timestamp,
	marked_indices,
	is_melee_attacking,
	seed
}).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([mark_update/2]).
-export([start_melee_attack/1, stop_melee_attack/1]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").
-include("include/attack.hrl").



%% public api
mark_update(Guid, Indices) when is_list(Indices) ->
	Pid = get_pid(Guid),
	gen_server:cast(Pid, {mark_update, Indices}).


start_melee_attack(Guid) ->
	Pid = get_pid(Guid),
	gen_server:cast(Pid, start_melee_attack).

stop_melee_attack(Guid) ->
	Pid = get_pid(Guid),
	gen_server:cast(Pid, stop_melee_attack).
	



%% behavior callbacks

start_link(AccountId, Guid) ->
	gen_server:start_link(?MODULE, {AccountId, Guid}, []).

init({AccountId, Guid}) ->
	process_flag(trap_exit, true),
	io:format("char SERVER started for ~p~n", [Guid]),

	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	Seed = {A,B,C},
	random:seed(Seed),

	% create session for this char
	char_sess:create(Guid),

	Key = build_pid_key(AccountId),
	gproc:reg({n, l, Key}, none),
	gproc:reg({n, l, Guid}, none),

	{ok, TRef} = timer:send_interval(?game_tick, update),
	{ok, #state{account_id=AccountId, guid=Guid, update_timer=TRef, timestamp=now(), last_swing=0, marked_indices=[], is_melee_attacking=false, seed=Seed}}.


handle_cast(start_melee_attack, State = #state{is_melee_attacking=IsAttacking, last_swing=LastSwing}) ->
	NewLastSwing = if IsAttacking -> LastSwing;
		not IsAttacking -> 0
	end,
	{noreply, State#state{is_melee_attacking=true, last_swing=NewLastSwing}};
handle_cast(stop_melee_attack, State) ->
	{noreply, State#state{is_melee_attacking=false, last_swing=0}};
handle_cast({mark_update, Indices}, State = #state{marked_indices=StoredIndices}) ->
	NewIndices = Indices ++ StoredIndices,
	{noreply, State#state{marked_indices=NewIndices}};
handle_cast(Msg, State) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, State}.


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.


handle_info(update, State = #state{guid=Guid, timestamp=Ts, last_swing=LastSwing, marked_indices=Indices, is_melee_attacking=IsAttacking, seed=Seed}) ->

	CurrentTs = now(),

	{NextLastSwing, NextSeed} = if IsAttacking ->
			% now_diff returns diff in microseconds
			Diff = timer:now_diff(CurrentTs, Ts) div 1000,

			CharValues = char_data:get_values(Guid),
			SwingTimer = char_values:get(base_attack_time, CharValues),
			if LastSwing >= SwingTimer ->
					{Swung, NewSeed} = melee:swing(Guid, Seed),
					NewLastSwing = if Swung -> 0;
						not Swung -> LastSwing
					end,
					{NewLastSwing, NewSeed};
				true ->
					{Diff + LastSwing, Seed}
			end;
		not IsAttacking ->
			{LastSwing, Seed}
	end,



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

	{noreply, State#state{timestamp=CurrentTs, last_swing=NextLastSwing, marked_indices=[], seed=NextSeed}};
handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, #state{guid=Guid, update_timer=Timer}) ->
	timer:cancel(Timer),
	char_sess:delete(Guid),
	io:format("WORLD: shutting down char: ~p~n", [Guid]),
	ok.



%%%%%%%%%%%
%% private

build_pid_key(AccountId) ->
	AccountId ++ "char".

get_pid(Guid) when is_number(Guid) ->
	gproc:lookup_pid({n, l, Guid});
get_pid(AccountId) when is_list(AccountId) ->
	Key = build_pid_key(AccountId),
	gproc:lookup_pid({n, l, Key}).
