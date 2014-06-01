-module(server).
-export([pong/1, null/1]).

-include("include/binary.hrl").


pong(PropList) ->
	Value = proplists:get_value(payload, PropList),
	<<Ping?L, _Latency?L>> = case Value of
		undefined -> throw(badarg);
		Value -> Value
	end,
	Opcode = opcode_patterns:getNumByAtom(smsg_pong),
	Msg = <<Opcode?W, Ping?L>>,
	world_socket_controller:send(Msg),
	ok.


null(_PropList) ->
	%Opcode = opcode_patterns:getNumByAtom(msg_null_action),
	%Msg = <<Opcode?W>>,
	%world_socket_controller:send(Msg),
	ok.
