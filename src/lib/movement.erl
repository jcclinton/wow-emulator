-module(movement).
-export([start_forward/1, null/0]).

-include("include/binary.hrl").



start_forward(PropList) ->
	Opcode = opcode_patterns:getNumByAtom(msg_move_start_forward),
	Payload = proplists:get_value(payload, PropList),
	<<Guid?L, X?f, Y?f, Z?f, O?f, Val1?L, Val2?L>> = Payload,
	%Msg = <<Opcode?W, Payload/binary>>,
	io:format("guid: ~p~nopcode: ~p~npayload: ~p~n", [Guid, Opcode, Payload]),
	io:format("pos: {~p,~p,~p,~p}~n", [X, Y, Z, O]),
	io:format("guid: ~p rest: {~p,~p}~n", [Opcode, Val1, Val2]),
	ok.

null() -> ok.
