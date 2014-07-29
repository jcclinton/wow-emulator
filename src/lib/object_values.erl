-module(object_values).

-export([get_byte_value/3,
				get_uint16_value/3,
				get_uint32_value/2,
				get_uint64_value/2,
				get_float_value/2,
				get_value/2]).
-export([set_byte_value/4,
				set_uint16_value/4,
				set_uint32_value/3,
				set_uint64_value/3,
				set_float_value/4]).
-export([set_key_values/2]).

-include("include/binary.hrl").


set_key_values({IndexName, Value, Type}, Values) ->
	%io:format("indexname: ~p~nvalue: ~p~n", [IndexName, Value]),
	case Type of
		uint32 ->
			set_uint32_value(IndexName, Value, Values);
		uint64 ->
			set_uint64_value(IndexName, Value, Values);
		uint16_0 ->
			set_uint16_value(IndexName, Value, Values, 0);
		byte_0 ->
			set_byte_value(IndexName, Value, Values, 0);
		byte_1 ->
			set_byte_value(IndexName, Value, Values, 1);
		byte_2 ->
			set_byte_value(IndexName, Value, Values, 2);
		byte_3 ->
			set_byte_value(IndexName, Value, Values, 3);
		float ->
			set_float_value(IndexName, Value, Values);
		{float, Offset} ->
			set_float_value(IndexName, Value, Values, Offset)
	end.


get_byte_value(IndexName, Values, Offset) ->
	get_value(IndexName, Values, 1, Offset).

get_uint16_value(IndexName, Values, Offset) ->
	get_value(IndexName, Values, 2, Offset).

get_uint32_value(IndexName, Values) ->
	get_value(IndexName, Values, 4, 0).

get_uint64_value(IndexName, Values) ->
	get_value(IndexName, Values, 8, 0).

get_float_value(IndexName, Values) ->
	get_value(IndexName, Values, float).

get_value(IndexName, Values, float) ->
	% each Index is a 4 byte long word
	Index = update_fields:fields(IndexName) * 4,
	<<_Head:Index/binary, Value?f, _Tail/binary>> = Values,
	Value.

get_value(Index, Values) ->
	get_uint32_value(Index, Values).
get_value(IndexName, Values, Size, Offset) when is_atom(IndexName) ->
	Index = update_fields:fields(IndexName),
	get_value(Index, Values, Size, Offset);
get_value(IndexIn, Values, Size, Offset) ->
	% each Index is a 4 byte long word
	Index = (IndexIn * 4) + Offset,
	BitSize = Size*8,
	<<_Head:Index/binary, Value:BitSize/unsigned-little-integer, _Tail/binary>> = Values,
	Value.


set_byte_value(IndexName, Value, Values, Offset) ->
	set_value(IndexName, Value, Values, 1, Offset).

set_uint16_value(IndexName, Value, Values, Offset) ->
	set_value(IndexName, Value, Values, 2, Offset).

set_uint32_value(IndexName, Value, Values) ->
	set_value(IndexName, Value, Values, 4, 0).

set_float_value(IndexName, Value, Values, Offset) ->
	set_value(IndexName, Value, Values, float, Offset).

set_uint64_value(IndexName, Value, Values) ->
	set_value(IndexName, Value, Values, 8, 0).

set_float_value(IndexName, Value, Values) ->
	set_value(IndexName, Value, Values, float).

set_value(IndexName, Value, Values, float) ->
	set_value(IndexName, Value, Values, float, 0).

set_value(IndexName, Value, Values, float, Offset) ->
	% each Index is a 4 byte long word
	Index = (update_fields:fields(IndexName) + Offset) * 4,
	<<Head:Index/binary, _OldValue:4/binary, Tail/binary>> = Values,
	<<Head:Index/binary, Value?f, Tail/binary>>;
set_value(IndexName, Value, Values, Size, Offset) when is_atom(IndexName) ->
	Index = update_fields:fields(IndexName),
	set_value(Index, Value, Values, Size, Offset);
set_value(IndexIn, Value, Values, Size, Offset) ->
	% each Index is a 4 byte long word
	Index =  4 * IndexIn + Offset,
	BitSize = Size * 8,
	<<Head:Index/binary, _OldValue:Size/binary, Tail/binary>> = Values,
	<<Head:Index/binary, Value:BitSize/unsigned-little-integer, Tail/binary>>.
