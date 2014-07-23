-module(movement).
-export([handle_movement/1, set_active_mover/1, stand_state_change/1, move_time_skipped/1]).
-export([move_fall_land/1]).

-include("include/binary.hrl").


-define(MAX_NUMBER_OF_GRIDS, 64).
-define(SIZE_OF_GRIDS, 533.33333).
-define(MAP_SIZE, ?SIZE_OF_GRIDS*?MAX_NUMBER_OF_GRIDS).
-define(MAP_HALFSIZE, ?MAP_SIZE/2).


move_fall_land(_Data) ->
	io:format("received req to move time land~n"),
	ok.


move_time_skipped(_Data) ->
	io:format("received req to move time skipped~n"),
	ok.

stand_state_change(_Data) ->
	io:format("received req to set stand state change~n"),
	ok.

set_active_mover(_Data) ->
	% dont need to do anything
	%io:format("received req to set active mover~n"),
	ok.

handle_movement(Data) ->
	Payload = recv_data:get(payload, Data),
	MoveData = move_info:read(Payload),

	{X, Y, Z, O} = move_info:get_coords(MoveData),
	Allowable = verify_movement(X, Y, Z, O),

	if Allowable ->
			OpAtom = recv_data:get(op_atom, Data),
			Guid = recv_data:get(guid, Data),
			PackGuid = <<7?B, Guid?G>>,
			NewPayload = move_info:write(MoveData),
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
