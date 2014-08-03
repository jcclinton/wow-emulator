-module(player_character).
-behavior(gen_server).

-record(state, {
	account_id,
	guid,
	update_timer,
	last_swing,
	timestamp,
	mask
}).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([update/1, handle_packet/4]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").
-include("include/attack.hrl").

-define(update_timer_interval, 50).


%% public api
handle_packet(AccountId, OpAtom, Callback, Payload) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {packet_rcvd, OpAtom, Callback, Payload}).

update(AccountId) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, update).


mark_update(Index, AccountId)



%% behavior callbacks

start_link(AccountId, Guid) ->
	gen_server:start_link(?MODULE, {AccountId, Guid}, []).

init({AccountId, Guid}) ->
	process_flag(trap_exit, true),
	io:format("char SERVER started for ~p~n", [Guid]),

	Key = build_pid_key(AccountId),
	gproc:reg({n, l, Key}, none),
	gproc:reg({n, l, Guid}, none),
	char_data:init_session(Guid),

	EmptyMask = update_mask:empty(),

	{ok, TRef} = timer:apply_interval(?update_timer_interval, ?MODULE, update, [AccountId]),
	{ok, #state{account_id=AccountId, guid=Guid, update_timer=TRef, timestamp=now(), last_swing=0, mask=EmptyMask}}.


handle_cast(update, State = #state{guid=Guid, timestamp=Ts, last_swing=LastSwing}) ->
	CurrentTs = now(),
	% now_diff returns diff in microseconds
	Diff = timer:now_diff(CurrentTs, Ts) div 1000,

	NextLastSwing = if LastSwing > 2000 ->
			AttackOpAtom = smsg_attackerstateupdate,

			HitInfo = ?hitinfo_normalswing,
			PackGuid = guid:pack(Guid),
			TargetGuid = 1046,
			TargetPackGuid = guid:pack(TargetGuid),
			Damage = 5,
			DamageSchoolMask = 0,
			Absorb = 0,
			Resist = 0,
			TargetState = ?victimstate_normal,
			Blocked = 0,

			Payload = <<HitInfo?L, PackGuid/binary, TargetPackGuid/binary, Damage?L, 1?B, DamageSchoolMask?L, Damage?f, Damage?L, Absorb?L, Resist?L, TargetState?L, 0?L, 0?L, Blocked?L>>,
			%world:send_to_all(AttackOpAtom, Payload),
			0;
		true ->
			Diff + LastSwing
	end,


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

	{noreply, State#state{timestamp=CurrentTs, last_swing=NextLastSwing}};
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
	timer:cancel(Timer),
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
	
