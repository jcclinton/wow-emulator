-module(character).
-export([enum/2]).

-include("include/binary.hrl").

enum(User, Payload) ->
	Pids = [self()],
	Opcode = opcode_patterns:getNumByAtom(smsg_char_enum),
	Num = 0,
	Msg = <<Opcode?W, Num?B>>,
	{User, {Pids, Msg}}.
