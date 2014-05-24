-module(server).
-export([pong/1, null/1]).

-include("include/binary.hrl").


pong(PropList) ->
	Value = proplists:get_value(payload, PropList),
	<<Ping?L, _Latency?L>> = case Value of
		undefined -> throw(badarg);
		Value -> Value
	end,
	Pids = [self()],
	Opcode = opcode_patterns:getNumByAtom(smsg_pong),
	Msg = <<Opcode?W, Ping?L>>,
	{[], {Pids, Msg}}.


null(_PropList) ->
	Pids = self(),
	Opcode = opcode_patterns:getNumByAtom(msg_null_action),
	Msg = <<Opcode?W>>,
	{[], {Pids, Msg}}.
