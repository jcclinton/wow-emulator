-module(player_character).
-behavior(gen_server).

-record(state, {
	account_id,
	guid,
	update_timer,
	last_swing,
	timestamp,
	marked_indices,
	is_melee_attacking
}).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([handle_packet/4, mark_update/2]).
-export([start_melee_attack/1, stop_melee_attack/1]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").
-include("include/attack.hrl").

-define(update_timer_interval, 50).


%% public api
handle_packet(AccountId, OpAtom, Callback, Payload) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {packet_rcvd, OpAtom, Callback, Payload}).


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

	% create session for this char
	char_sess:create(Guid),

	Key = build_pid_key(AccountId),
	gproc:reg({n, l, Key}, none),
	gproc:reg({n, l, Guid}, none),

	{ok, TRef} = timer:send_interval(?update_timer_interval, update),
	{ok, #state{account_id=AccountId, guid=Guid, update_timer=TRef, timestamp=now(), last_swing=0, marked_indices=[], is_melee_attacking=false}}.


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
handle_cast({packet_rcvd, OpAtom, Callback, Payload}, State = #state{account_id=AccountId, guid=Guid}) ->
	Args = [{op_atom, OpAtom}, {guid, Guid}, {payload, Payload}, {account_id, AccountId}],
	player_workers_sup:start_worker({Callback, Args}, AccountId),
	{noreply, State};
handle_cast(Msg, State) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, State}.


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.


handle_info(update, State = #state{guid=Guid, timestamp=Ts, last_swing=LastSwing, marked_indices=Indices, is_melee_attacking=IsAttacking}) ->

	CurrentTs = now(),

	NextLastSwing = if IsAttacking ->
			% now_diff returns diff in microseconds
			Diff = timer:now_diff(CurrentTs, Ts) div 1000,

			if LastSwing > 2000 ->
					AttackOpAtom = smsg_attackerstateupdate,

					HitInfo = ?hitinfo_normalswing,
					PackGuid = guid:pack(Guid),
					TargetGuid = char_sess:get_target(Guid),
					TargetPackGuid = guid:pack(TargetGuid),
					Damage = 5,
					DamageSchoolMask = 0,
					Absorb = 0,
					Resist = 0,
					TargetState = ?victimstate_normal,
					Blocked = 0,

					Payload = <<HitInfo?L, PackGuid/binary, TargetPackGuid/binary, Damage?L, 1?B, DamageSchoolMask?L, Damage?f, Damage?L, Absorb?L, Resist?L, TargetState?L, 0?L, 0?L, Blocked?L>>,
					world:send_to_all(AttackOpAtom, Payload),
					0;
				true ->
					Diff + LastSwing
			end;
		not IsAttacking ->
			LastSwing
	end,



	% if any values have been changed, do update
	Len = length(Indices),
	if Len > 0 ->
			Mask = lists:foldl(fun(Index, MaskAcc) ->
				update_mask:set_bit(Index, MaskAcc)
			end, update_mask:empty_player(), Indices),

			Values = char_data:get_values(Guid),
			{OpAtom, Msg} = update_data:build_update_packet(Mask, Values),
			world:send_to_all(OpAtom, Msg),
			ok;
		Len == 0 -> ok
	end,

	{noreply, State#state{timestamp=CurrentTs, last_swing=NextLastSwing, marked_indices=[]}};
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
