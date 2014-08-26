-module(player_state).
-behavior(gen_server).

-record(state, {
	guid,
	values
}).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_value/2, get_values/2, set_value/3]).
-export([get_values/1, set_values/2]).


-include("include/binary.hrl").
-include("include/database_records.hrl").


get_values(Guid, Fields) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:call(Pid, {get_multiple, Fields}).

get_value(Guid, Field) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:call(Pid, {get, Field}).

set_value(Guid, Value, Field) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:cast(Pid, {set, Value, Field}).

% temp
get_values(Guid) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:call(Pid, get_all).

set_values(Guid, Values) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_server:cast(Pid, {set_all, Values}).



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
handle_call(_E, _From, State) ->
	{noreply, State}.


handle_cast({set_all, NewValues}, State = #state{}) ->
	{noreply, State#state{values=NewValues}};
handle_cast({set, Value, Field}, State = #state{values=Values}) ->
	NewValues = char_values:set(Field, Value, Values),
	{noreply, State#state{values=NewValues}};
handle_cast(_, State) ->
	{noreply, State}.



% recive challenge
handle_info(Msg, State) ->
	io:format("player state: received unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	io:format("player state: closing~n"),
	ok.
