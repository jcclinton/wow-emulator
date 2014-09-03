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

-module(player_state).
-behavior(gen_server).

-record(state, {
	guid,
	values
}).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_value/2, get_values/2, set_value/3]).
-export([get_values/1]).
-export([set_multiple_values/2]).
-export([run_sync_function/2, run_sync_function/3]).
-export([run_async_function/2, run_async_function/3]).
-export([update/0]).


-include("include/binary.hrl").
-include("include/database_records.hrl").



%% public api
update() -> ok.

% get multiple values
get_values(Guid, Fields) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:call(Pid, {get_multiple, Fields}).

% get single value
get_value(Guid, FieldData) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:call(Pid, {get, FieldData}).

% set single value
set_value(Guid, Value, FieldData) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:cast(Pid, {set, Value, FieldData}).

% set multiple values
set_multiple_values(Guid, PropList) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:cast(Pid, {set_multiple, PropList}).

% run sync function from within this process
run_sync_function(Guid, FuncName) ->
	run_sync_function(Guid, FuncName, []).
run_sync_function(Guid, FuncName, Args) when is_list(Args) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:call(Pid, {run_func, FuncName, Args}).

% run async function form within this process
run_async_function(Guid, FuncName) ->
	run_async_function(Guid, FuncName, []).
run_async_function(Guid, FuncName, Args) when is_list(Args) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:cast(Pid, {run_func, FuncName, Args}).


% TODO optimize any code where these are used to be more efficient
% the values binary should not be passed around between processes
% except in a more controlled fashion
% gets entire values object
get_values(Guid) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:call(Pid, get_all).



%% behavior callbacks

start_link(Guid) ->
	gen_server:start_link(?MODULE, Guid, []).

init(Guid) ->
	util:reg_proc(?MODULE, Guid),
	Values = char_data:get_stored_values(Guid),
	player_model_controller:login_complete(Guid),
	{ok, #state{guid=Guid, values=Values}}.


handle_call({get_multiple, Fields}, _From, State = #state{values=Values}) ->
	ValueList = lists:foldl(fun(FieldData, Acc) ->
		Value = char_values:get_value(FieldData, Values),
		[{FieldData, Value}|Acc]
	end, [], Fields),
	{reply, ValueList, State};
handle_call(get_all, _From, State = #state{values=Values}) ->
	{reply, Values, State};
handle_call({get, FieldData}, _From, State = #state{values=Values}) ->
	Value = char_values:get_value(FieldData, Values),
	{reply, Value, State};
handle_call({run_func, FuncName, Args}, _From, State = #state{values=Values}) ->
	% this will run custom functions within the player_state process
	% call is run as a get
	NewArgs = [Values|Args],
	M = player_state_functions,
	Response = apply(M, FuncName, NewArgs),
	{reply, Response, State};
handle_call(_E, _From, State) ->
	{noreply, State}.


handle_cast({set_multiple, PropList}, State = #state{values=Values, guid=Guid}) ->
	{NewValues, Indices} = lists:foldl(fun({FieldName, Value}, {AccValues, AccIndices}) ->
		{OutValues, OutIndices} = char_values:set_value(FieldName, Value, AccValues),
		{OutValues, OutIndices ++ AccIndices}
	end, {Values, []}, PropList),
	unit_updater:mark_update(Guid, Indices),
	char_data:update_char_values(Guid, NewValues),
	{noreply, State#state{values=NewValues}};
handle_cast({set, Value, FieldData}, State = #state{values=Values, guid=Guid}) ->
	{NewValues, Indices} = char_values:set_value(FieldData, Value, Values),
	unit_updater:mark_update(Guid, Indices),
	char_data:update_char_values(Guid, NewValues),
	{noreply, State#state{values=NewValues}};
handle_cast({run_func, FuncName, Args}, State = #state{values=Values, guid=Guid}) ->
	% this will run custom functions within the player_state process
	% cast is run as a set
	NewArgs = [Values|Args],
	M = player_state_functions,
	{UpdatedValues, Indices} = apply(M, FuncName, NewArgs),
	NewValues = if is_binary(UpdatedValues) ->
			OldSize = byte_size(Values),
			NewSize = byte_size(UpdatedValues),
			% double check this is a valid values object we have
			if OldSize == NewSize ->
					unit_updater:mark_update(Guid, Indices),
					char_data:update_char_values(Guid, UpdatedValues),
					UpdatedValues;
				OldSize /= NewSize ->
					Values
			end;
		true -> Values
	end,
	{noreply, State#state{values=NewValues}};
handle_cast(_, State) ->
	{noreply, State}.


handle_info(Msg, State) ->
	io:format("player state: received unknown message: ~p~n", [Msg]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	io:format("player state: closing~n"),
	ok.
