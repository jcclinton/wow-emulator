-module(character).
-export([enum/1]).

-include("include/binary.hrl").

enum(_PropList) ->
	Pids = [self()],
	Opcode = opcode_patterns:getNumByAtom(smsg_char_enum),
	Num = 0,
	Msg = <<Opcode?W, Num?B>>,
	{[], {Pids, Msg}}.
