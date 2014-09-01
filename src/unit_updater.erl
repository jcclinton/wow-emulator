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
mark_update(_, []) ->
	% ignore empty updates
	ok;
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

	Values = player_state:get_values(Guid),
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
