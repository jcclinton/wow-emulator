-module(move_info).

-export([write/1, read/1]).
-export([get_coords/1]).

-include("include/binary.hrl").


write(MoveData) ->
	{X, Y, Z, O} = get_coords(MoveData),
	MoveFlags = get_value(flags, MoveData),
	Time = get_value(time, MoveData),
	<<MoveFlags?L, Time?L, X?f, Y?f, Z?f, O?f>>.


read(Payload) ->
	<<MoveFlags?L, Time?L, X?f, Y?f, Z?f, O?f, Unk1?L>> = Payload,
	[{flags, MoveFlags}, {time, Time}, {coords, {X, Y, Z, O}}, {unk1, Unk1}].


get_coords(MoveData) ->
	get_value(coords, MoveData).


% private
get_value(Type, MoveData) ->
	proplists:get_value(Type, MoveData).
