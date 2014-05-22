-module(opcode_patterns).

-export([lookup/1]).


lookup(16#0037) -> {movement, move};
lookup(16#01DC) -> {movement, move};
lookup(Unk) -> io:format("unknown opcode: ~p~n", [Unk]).
