-module(movement).
-export([handle_movement/1, null/0]).

-include("include/binary.hrl").



handle_movement(PropList) ->
	Opcode = opcode_patterns:getNumByAtom(msg_move_start_forward),
	Payload = proplists:get_value(payload, PropList),
	<<MoveFlags?L, Time?L, X?f, Y?f, Z?f, O?f, Unk1?L>> = Payload,
	%Msg = <<Opcode?W, Payload/binary>>,
	io:format("moveflags: ~p~ntime: ~p~nopcode: ~p~npayload: ~p~n", [MoveFlags, Time, Opcode, Payload]),
	io:format("pos: {~p,~p,~p,~p}~n", [X, Y, Z, O]),
	io:format("opcode: ~p rest: {~p}~n", [Opcode, Unk1]),
	ok.

null() -> ok.
