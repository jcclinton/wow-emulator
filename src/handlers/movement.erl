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

-module(movement).
-export([handle_movement/1]).
-export([set_active_mover/1, stand_state_change/1, move_time_skipped/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


-define(MAX_NUMBER_OF_GRIDS, 64).
-define(SIZE_OF_GRIDS, 533.33333).
-define(MAP_SIZE, ?SIZE_OF_GRIDS*?MAX_NUMBER_OF_GRIDS).
-define(MAP_HALFSIZE, ?MAP_SIZE/2).



move_time_skipped(_Data) ->
	% this is used if you need to modify last move time
	% but not needed for now

	%<<Guid?Q, Time?L>> = recv_data:get(payload, Data),
	%then subtract Time from last move time maybe?
	
	ok.

stand_state_change(Data) ->
	Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<AnimState?B, _/binary>> = Payload,
	player_state:set_value(Guid, AnimState, anim_state),
	{smsg_standstate_update, <<AnimState?B>>}.


set_active_mover(_Data) ->
	% dont need to do anything
	ok.

handle_movement(Data) ->
	Payload = recv_data:get(payload, Data),
	MoveData = move_info:read(Payload),

	{X, Y, Z, O} = move_info:get_coords(MoveData),
	Allowable = verify_movement(X, Y, Z, O),

	if Allowable ->
			OpAtom = recv_data:get(op_atom, Data),
			Guid = recv_data:get(guid, Data),

			char_data:update_coords(Guid, X, Y, Z, O, MoveData),
			%io:format("o: ~p~n", [O]),
			%io:format("coords: ~p, ~p, ~p~n", [X, Y, Z]),

			PackGuid = <<7?B, Guid?G>>,
			Time = move_info:get_value(time, MoveData),
			NewTime = Time + 500,
			MoveData2 = move_info:update(time, NewTime, MoveData),
			NewPayload = move_info:write(MoveData2),
			Msg = <<PackGuid/binary, NewPayload/binary>>,
			world:send_to_all_but_player(OpAtom, Msg, Guid);
		not Allowable ->
			io:format("bad movement data passed in: ~p~n", [Payload]),
			ok
	end,
	ok.




%%%%%%%%%%%%%%%%%%%%%
%% private

verify_movement(X, Y, Z, O) ->
	finite(O) andalso verify_movement(X, Y, Z).

verify_movement(X, Y, Z) ->
	finite(Z) andalso verify_movement(X, Y).

verify_movement(X, Y) ->
	verify_movement(X) andalso verify_movement(Y).

verify_movement(C) ->
	finite(C) andalso (abs(C) =< ?MAP_HALFSIZE - 0.5).
	



% just check if its a 32 bit number
%finite(C) -> C band 16#00000000 == 0.
finite(_) -> true.
