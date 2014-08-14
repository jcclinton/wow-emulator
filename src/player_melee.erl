-module(player_melee).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).

-export([idle/2, attacking/2]).
-export([start_melee_attack/1, stop_melee_attack/1]).
-export([update/0]).

-include("include/binary.hrl").


-record(state, {
	guid,
	timer_swing,
	timer,
	seed
}).

%% public api
start_melee_attack(Guid) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_fsm:send_event(Pid, start).

stop_melee_attack(Guid) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_fsm:send_event(Pid, stop).

update() -> ok.


%% behavior callbacks

start_link(Guid) ->
	gen_fsm:start_link(?MODULE, {Guid}, []).

init({Guid}) ->
	io:format("starting player melee~n"),

	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	Seed = {A,B,C},
	random:seed(Seed),

	util:reg_proc(?MODULE, Guid),

	{ok, idle, #state{guid=Guid, timer=none, seed=Seed}}.


idle(start, State = #state{guid=Guid}) ->
	Timer = schedule_swing(Guid),

	{next_state, attacking, State#state{timer=Timer}};
idle(_, State) ->
	{next_state, idle, State}.

attacking(stop, State = #state{timer=Timer}) ->
	timer:cancel(Timer),
	{next_state, idle, State#state{timer=none}};
attacking(swing, State = #state{guid=Guid, seed=Seed}) ->
	{_Swung, NewSeed} = melee:swing(Guid, Seed),

	Timer = schedule_swing(Guid),

	{next_state, attacking, State#state{timer=Timer, seed=NewSeed}};
attacking(_, State) ->
	{next_state, idle, State}.


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



%%%%%%%%%%%
%% private

schedule_swing(Guid) ->
	TimerSwing = char_values:get(base_attack_time, Guid),
	TimerSwingInt = round(TimerSwing),
	{ok, Timer} = timer:apply_after(TimerSwingInt, gen_fsm, send_event, [self(), swing]),
	Timer.
