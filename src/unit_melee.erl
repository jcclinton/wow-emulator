%%   This is a World of Warcraft emulator written in erlang, supporting
%%   client 1.12.x
%%
%%   Copyright (C) 2014  Jamie Clinton <jamieclinton.com>
%%
%%   This program is free software; you can redistribute it and/or modify
%%   it under the terms of the GNU General Public License as published by
%%   the Free Software Foundation; either version 2 of the License, or
%%   (at your option) any later version.
%%
%%   This program is distributed in the hope that it will be useful,
%%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%   GNU General Public License for more details.
%%
%%   You should have received a copy of the GNU General Public License along
%%   with this program; if not, write to the Free Software Foundation, Inc.,
%%   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%
%%   World of Warcraft, and all World of Warcraft or Warcraft art, images,
%%   and lore ande copyrighted by Blizzard Entertainment, Inc.

-module(unit_melee).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([
    init/1,
    handle_sync_event/4,
    handle_event/3,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-export([idle/2, attacking/2, attacking_out_of_range/2]).
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
    io:format("starting unit melee~n"),

    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    Seed = {A, B, C},
    rand:seed(exsss, Seed),

    util:reg_proc(?MODULE, Guid),

    {ok, idle, #state{guid = Guid, timer = none, seed = Seed}}.

idle(start, State = #state{guid = Guid}) ->
    Timer = schedule_swing(Guid),

    {next_state, attacking, State#state{timer = Timer}};
idle(_, State) ->
    {next_state, idle, State}.

attacking(stop, State = #state{timer = Timer}) ->
    timer:cancel(Timer),
    {next_state, idle, State#state{timer = none}};
attacking(swing, State = #state{guid = Guid, seed = Seed}) ->
    {_Swung, NewSeed} = melee:swing(Guid, Seed),

    Timer = schedule_swing(Guid),

    {next_state, attacking, State#state{timer = Timer, seed = NewSeed}};
attacking(_, State) ->
    {next_state, idle, State}.

attacking_out_of_range(_, State) ->
    {next_state, attacking_out_of_range, State}.

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
    TimerSwing = player_state:get_value(Guid, unit_field_baseattacktime),
    TimerSwingInt = round(TimerSwing),
    {ok, Timer} = timer:apply_after(TimerSwingInt, gen_fsm, send_event, [self(), swing]),
    Timer.
