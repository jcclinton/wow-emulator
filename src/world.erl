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

-module(world).
-behavior(gen_server).

-record(state, {players = []}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_guid/2]).
-export([add_to_map/1, remove_from_map/1]).
-export([send_to_all_but_player/3, send_to_all/2]).

-include("include/binary.hrl").
-include("include/database_records.hrl").

%%% public api
get_guid(HighGuid, Entry) ->
    gen_server:call(?MODULE, {new_guid, HighGuid, Entry}).

add_to_map(Player) ->
    gen_server:call(?MODULE, {add_to_map, Player}).

remove_from_map(Guid) ->
    gen_server:call(?MODULE, {remove_from_map, Guid}).

send_to_all_but_player(OpAtom, Payload, Guid) ->
    gen_server:cast(?MODULE, {send_to_all_but_player, OpAtom, Payload, Guid}).

send_to_all(OpAtom, Payload) ->
    gen_server:cast(?MODULE, {send_to_all_but_player, OpAtom, Payload, 0}).

%% gen_server callbacks

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("world: started~n"),
    {ok, #state{}}.

handle_call({add_to_map, NewPlayer = {AccountId, Guid}}, _From, State = #state{players = Players}) ->
    %login packets to send when player is added to map
    {OpAtom1, Update1} = update_data:build_create_update_packet_for_player(Guid, true),
    player_controller:send(AccountId, OpAtom1, Update1),

    % send update to other players
    {OpAtom2, Update2} = update_data:build_create_update_packet_for_player(Guid, false),
    send_to_all_but_player(OpAtom2, Update2, Guid),

    InList = lists:any(fun({_, GuidOther}) -> GuidOther == Guid end, Players),
    %add player to list
    NewPlayers =
        if
            InList ->
                Players;
            not InList ->
                % send updates about other players to new player
                lists:foreach(
                    fun({_, GuidOther}) ->
                        {OpAtom, Update} = update_data:build_create_update_packet_for_player(
                            GuidOther, false
                        ),
                        player_controller:send(AccountId, OpAtom, Update)
                    end,
                    Players
                ),
                [NewPlayer | Players]
        end,

    {reply, ok, State#state{players = NewPlayers}};
handle_call({remove_from_map, RemoveGuid}, _From, State = #state{players = Players}) ->
    NewPlayers = lists:foldl(
        fun(Player = {_, Guid}, Acc) ->
            if
                Guid == RemoveGuid -> Acc;
                Guid /= RemoveGuid -> [Player | Acc]
            end
        end,
        [],
        Players
    ),
    send_to_all_but_player(smsg_destroy_object, <<RemoveGuid ?Q>>, RemoveGuid),
    {reply, ok, State#state{players = NewPlayers}};
handle_call({new_guid, HighGuid, Entry}, _From, State) ->
    LowGuid = world_data:increment_guid(),
    Guid = guid:format(HighGuid, Entry, LowGuid),
    {reply, Guid, State};
handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(
    {send_to_all_but_player, OpAtom, Payload, ExcludeGuid}, State = #state{players = Players}
) ->
    % inform other players in list
    lists:foreach(
        fun({AccountId, Guid}) ->
            if
                Guid /= ExcludeGuid ->
                    player_controller:send(AccountId, OpAtom, Payload);
                Guid == ExcludeGuid ->
                    ok
            end
        end,
        Players
    ),
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
