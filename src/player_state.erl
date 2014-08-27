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


-include("include/binary.hrl").
-include("include/database_records.hrl").



%% public api

% get multiple values
get_values(Guid, Fields) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:call(Pid, {get_multiple, Fields}).

% get single value
get_value(Guid, Field) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:call(Pid, {get, Field}).

% set single value
set_value(Guid, Value, Field) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:cast(Pid, {set, Value, Field}).

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
	ValueList = lists:foldl(fun(Field, Acc) ->
		Value = char_values:get(Field, Values),
		[{Field, Value}|Acc]
	end, [], Fields),
	{reply, ValueList, State};
handle_call(get_all, _From, State = #state{values=Values}) ->
	{reply, Values, State};
handle_call({get, Field}, _From, State = #state{values=Values}) ->
	Value = char_values:get(Field, Values),
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


handle_cast({set_multiple, PropList}, State = #state{values=Values}) ->
	NewValues = lists:foldl(fun({Field, Value}, AccValues) ->
		char_values:set(Field, Value, AccValues)
	end, Values, PropList),
	{noreply, State#state{values=NewValues}};
handle_cast({set, Value, Field}, State = #state{values=Values}) ->
	NewValues = char_values:set(Field, Value, Values),
	{noreply, State#state{values=NewValues}};
handle_cast({run_func, FuncName, Args}, State = #state{values=Values}) ->
	% this will run custom functions within the player_state process
	% cast is run as a set
	NewArgs = [Values|Args],
	M = player_state_functions,
	UpdatedValues = apply(M, FuncName, NewArgs),
	NewValues = if is_binary(UpdatedValues) -> UpdatedValues;
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
