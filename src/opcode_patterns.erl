-module(opcode_patterns).

-export([getCallbackByNum/1, getNumByAtom/1]).


getCallbackByNum(16#0037) -> {character, enum};
getCallbackByNum(16#01DC) -> {server, pong};
getCallbackByNum(Unk) ->
	io:format("unknown opcode: ~p~n", [Unk]),
	{server, null}.


getNumByAtom(msg_null_action) -> 16#000;
getNumByAtom(smsg_char_enum) -> 16#03B;
getNumByAtom(smsg_pong) -> 16#1DD;
getNumByAtom(Unk) ->
	io:format("unknown opcode: ~p~n", [Unk]),
	0.
