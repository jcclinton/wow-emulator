-module(server).
-export([pong/2]).

-include("include/binary.hrl").


pong(User, <<Ping?L, _Latency?L>>) ->
	Pids = [self()],
	Opcode = opcode_patterns:getNumByAtom(smsg_pong),
	Msg = <<Opcode?W, Ping?L>>,
	{User, {Pids, Msg}}.
