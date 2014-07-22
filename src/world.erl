-module(world).
-behavior(gen_server).

-record(state, {
								players = []
							 }).


-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_guid/0]).
-export([get_pid/1]).
-export([build_pid/1, build_pid/2]).
-export([add_to_map/1, remove_from_map/1, get_map_players/0]).
-export([send_to_all_but_player/3]).


-include("include/binary.hrl").
-include("include/database_records.hrl").

%%% public api
get_guid() ->
	gen_server:call(?MODULE, new_guid).

add_to_map(Name) ->
	gen_server:call(?MODULE, {add_to_map, Name}).

remove_from_map(Name) ->
	gen_server:call(?MODULE, {remove_from_map, Name}).

get_map_players() ->
	gen_server:call(?MODULE, get_map_players).

send_to_all_but_player(OpAtom, Payload, Player) ->
	gen_server:cast(?MODULE, {send_to_all_but_player, OpAtom, Payload, Player}).


get_pid(Pid) when is_pid(Pid) -> Pid;
	% only atoms can be used as names locally
	% so this is global so we dont have to dynamically generate atoms
get_pid(Name) when is_list(Name) -> build_pid(Name).

% used to create new named processes
build_pid(Name) -> build_pid(Name, "").
build_pid(Name, Suffix) ->
	NewName = Name ++ "_" ++ Suffix,
	{global, NewName}.



%% gen_server callbacks

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("world: started~n"),
	{ok, #state{}}.


handle_call({add_to_map, Name}, _From, State = #state{players=Players}) ->
	InList = lists:any(fun(Player) -> Player == Name end, Players),

	%add player to list
	NewPlayers = if InList -> Players;
		not InList ->
			[Name|Players]
	end,

	{reply, ok, State#state{players=NewPlayers}};
handle_call({remove_from_map, Name}, _From, State = #state{players=Players}) ->
	NewPlayers = lists:delete(Name, Players),
	{reply, ok, State#state{players=NewPlayers}};
handle_call(get_map_players, _From, State = #state{players=Players}) ->
	{reply, Players, State};
handle_call(new_guid, _From, State) ->
	Guid = world_data:increment_guid(),
	{reply, Guid, State};
handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast({send_to_all_but_player, OpAtom, Payload, Name}, State = #state{players=Players}) ->
	% inform other players in list
	lists:foreach(fun(Player) ->
		if Player /= Name ->
				player_controller:send(Player, OpAtom, Payload);
			Player == Name -> ok
		end
	end, Players),
	{noreply, State};
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
