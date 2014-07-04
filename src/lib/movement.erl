-module(movement).
-export([handle_movement/1, set_active_mover/1, stand_state_change/1, move_time_skipped/1]).
-export([move_fall_land/1]).
-export([null/0]).

-include("include/binary.hrl").



move_fall_land(PropList) ->
	io:format("received req to move time land~n"),
	ok.


move_time_skipped(PropList) ->
	io:format("received req to move time skipped~n"),
	ok.

stand_state_change(PropList) ->
	io:format("received req to set stand state change~n"),
	ok.

set_active_mover(PropList) ->
	io:format("received req to set active mover~n"),
	ok.

handle_movement(PropList) ->
	Opcode = opcode_patterns:getNumByAtom(msg_move_start_forward),
	Payload = proplists:get_value(payload, PropList),
	<<MoveFlags?L, Time?L, X?f, Y?f, Z?f, O?f, Unk1?L>> = Payload,
	%Msg = <<Opcode?W, Payload/binary>>,
	%io:format("moveflags: ~p~ntime: ~p~nopcode: ~p~npayload: ~p~n", [MoveFlags, Time, Opcode, Payload]),
	io:format("moveflags: ~p pos: {~p,~p,~p,~p}~n", [MoveFlags, X, Y, Z, O]),
	%io:format("opcode: ~p rest: {~p}~n", [Opcode, Unk1]),
	ok.

null() -> ok.
