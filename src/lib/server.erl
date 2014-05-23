-module(server).
-export([pong/1, noop/1]).

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


noop(_PropList) ->
	{[], {[], <<>>}}.
