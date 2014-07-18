-module(world).
-behavior(gen_server).

-record(state, {
								current_guid,
								players = []
							 }).


-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_guid/0]).
-export([add_to_map/1, remove_from_map/1, get_map_players/0]).


-include("include/binary.hrl").
-include("include/database_records.hrl").

%%% public api
get_guid() ->
	gen_server:call(?MODULE, new_guid).

add_to_map(Name) ->
	gen_server:cast(?MODULE, {add_to_map, Name}).

remove_from_map(Name) ->
	gen_server:cast(?MODULE, {remove_from_map, Name}).

get_map_players() ->
	gen_server:call(?MODULE, get_map_players).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("world: started~n"),
	{ok, #state{current_guid=1}}.


handle_call(get_map_players, _From, State = #state{players=Players}) ->
	{reply, Players, State};
handle_call(new_guid, _From, State = #state{current_guid=Guid}) ->
	{reply, Guid, State#state{current_guid=Guid+1}};
handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast({add_to_map, Name}, State = #state{players=Players}) ->
	InList = lists:any(fun(Player) -> Player == Name end, Players),
	NewPlayers = if InList -> Players;
		not InList -> [Name|Players]
	end,
	{noreply, State#state{players=NewPlayers}};
handle_cast({remove_from_map, Name}, State = #state{players=Players}) ->
	NewPlayers = lists:delete(Name, Players),
	{noreply, State#state{players=NewPlayers}};
handle_cast(_Msg, State) ->
	{noreply, State}.



handle_info(upgrade, State) ->
	%% loads latest code
	?MODULE:handle_info(do_upgrade, State),
	{noreply, State};
handle_info(_Msg, State) ->
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
