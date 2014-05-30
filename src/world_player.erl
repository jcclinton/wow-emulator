-module(world_player).
-behavior(gen_server).

-record(state, {
	value_count,
	values
							 }).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-compile([export_all]).


-include("include/binary.hrl").


set_byte_value(Pid, Index, Offset, Value) ->
	gen_server:cast(Pid, {set_byte, Index, Offset, Value}).

set_uint32_value(Pid, Index, Value) ->
	gen_server:cast(Pid, {set_uint32, Index, Value}).


start_link() ->
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	io:format("player SERVER: started~n"),
	Total = 16#446 + 16#b6 + 16#06,
	Values = build_values(Total),
	{ok, #state{value_count = Total, values=Values}}.


handle_call(_E, _From, State) ->
	{reply, ok, State}.

handle_cast({set_byte, Index, Offset, Value}, State) ->
	{noreply, State};
handle_cast({set_uint32, Index, Value}, State=#state{values=Values}) ->
	NewValues = replace_uint32(Index, Value, Values),
	{noreply, State#state{values=NewValues}};
handle_cast(Msg, S) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, S}.

handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

%% private

build_values(TotalWords) ->
	TotalBytes = 4 * TotalWords,
	lists:foldl(fun(_, AccIn) ->
		[<<0?B>> | AccIn]
	end, [], lists:seq(1, TotalBytes)).

replace_uint32(Index, Value, Values) ->
	replace_uint32((Index-1) * 4,Value,Values, 0, []).
replace_uint32(Index, <<V1?B, V2?B, V3?B, V4?B>>, Values, CurrentIndex, RevPrevious) when Index == CurrentIndex ->
	Tail = lists:nthtail(4, Values),
	Previous = lists:reverse(RevPrevious),
	Previous ++ [<<V1?B>>] ++ [<<V2?B>>] ++ [<<V3?B>>] ++ [<<V4?B>>] ++ Tail;
replace_uint32(Index, Value, [ThisValue|RestValues], CurrentIndex, Previous) ->
	replace_uint32(Index, Value, RestValues, CurrentIndex+1, [ThisValue|Previous]).
