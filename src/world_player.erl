-module(world_player).
-behavior(gen_server).

-record(state, {
	char
							 }).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-compile([export_all]).


-include("include/binary.hrl").


set_byte_value(Pid, Field, Offset, Value) ->
	gen_server:cast(Pid, {set_byte, Field, Offset, Value}).

set_uint32_value(Pid, Field, Value) ->
	gen_server:cast(Pid, {set_uint32, Field, Value}).


start_link() ->
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	io:format("player SERVER: started~n"),
	{ok, #state{}}.


handle_call(_E, _From, State) ->
	{reply, ok, State}.

handle_cast({set_byte, Field, Offset, Value}, State) ->
	{noreply, State};
handle_cast({set_uint32, Field, Value}, State) ->
	{noreply, State};
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
