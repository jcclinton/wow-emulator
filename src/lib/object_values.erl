-module(object_values).

-export([get_byte_value/3,
				get_uint16_value/3,
				get_uint32_value/2,
				get_int32_value/2,
				get_uint64_value/2,
				get_float_value/2,
				get_value/2]).
-export([set_byte_value/4,
				set_uint16_value/4,
				set_uint32_value/3,
				set_int32_value/3,
				set_uint64_value/3,
				set_float_value/4]).
-export([set_key_values/2]).

-include("include/binary.hrl").


% used when initializing char values
set_key_values({IndexName, Value, Type}, Values) ->
	%io:format("indexname: ~p~nvalue: ~p~n", [IndexName, Value]),
	case Type of
		int32 ->
			set_int32_value(IndexName, Value, Values);
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

get_int32_value(IndexName, Values) ->
	get_value(IndexName, Values, 4, 0, int).

get_uint32_value(IndexName, Values) ->
	get_value(IndexName, Values, 4, 0).

get_uint64_value(IndexName, Values) ->
	get_value(IndexName, Values, 8, 0).

get_float_value(IndexName, Values) ->
	get_value(IndexName, Values, float).


get_value(Field, Values, float) when is_atom(Field) ->
	Index = update_fields:fields(Field),
	get_value(Index, Values, float);
get_value(IndexIn, Values, float) ->
	% each Index is a 4 byte long word
	Index = IndexIn * 4,
	<<_Head:Index/binary, Value?f, _Tail/binary>> = Values,
	Value.

get_value(Index, Values) ->
	get_uint32_value(Index, Values).

get_value(IndexName, Values, Size, Offset) ->
	get_value(IndexName, Values, Size, Offset, uint).
get_value(IndexName, Values, Size, Offset, Type) when is_atom(IndexName) ->
	Index = update_fields:fields(IndexName),
	get_value(Index, Values, Size, Offset, Type);
get_value(IndexIn, Values, Size, Offset, Type) ->
	% each Index is a 4 byte long word
	Index = (IndexIn * 4) + Offset,
	BitSize = Size*8,
	if Type == int ->
			<<_Head:Index/binary, Value:BitSize/signed-little-integer, _Tail/binary>> = Values,
			Value;
		Type == uint ->
			<<_Head:Index/binary, Value:BitSize/unsigned-little-integer, _Tail/binary>> = Values,
			Value
	end.


set_byte_value(IndexName, Value, Values, Offset) ->
	if Offset > 3 orelse Offset < 0 -> throw(badarg);
		true -> ok
	end,
	if Value > 16#FF orelse Value < 0 -> throw(badarg);
		true -> ok
	end,
	set_value(IndexName, Value, Values, 1, Offset).

set_uint16_value(IndexName, Value, Values, Offset) ->
	if Offset > 1 orelse Offset < 0 -> throw(badarg);
		true -> ok
	end,
	if Value > 16#FFFF orelse Value < 0 -> throw(badarg);
		true -> ok
	end,
	set_value(IndexName, Value, Values, 2, Offset).

set_int32_value(IndexName, Value, Values) ->
	if Value > 16#FFFFFFFF -> throw(badarg);
		true -> ok
	end,
	set_value(IndexName, Value, Values, 4, 0, int).

set_uint32_value(IndexName, Value, Values) ->
	if Value > 16#FFFFFFFF orelse Value < 0 -> throw(badarg);
		true -> ok
	end,
	set_value(IndexName, Value, Values, 4, 0).

set_float_value(IndexName, Value, Values, Offset) ->
	set_value(IndexName, Value, Values, float, Offset).

set_uint64_value(IndexName, Value, Values) ->
	if Value > 16#FFFFFFFFFFFFFFFF orelse Value < 0 -> throw(badarg);
		true -> ok
	end,
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
set_value(IndexIn, Value, Values, Size, Offset) ->
	set_value(IndexIn, Value, Values, Size, Offset, uint).

set_value(IndexName, Value, Values, Size, Offset, Type) when is_atom(IndexName) ->
	Index = update_fields:fields(IndexName),
	set_value(Index, Value, Values, Size, Offset, Type);
set_value(IndexIn, Value, Values, Size, Offset, Type) ->
	% each Index is a 4 byte long word
	Index =  4 * IndexIn + Offset,
	BitSize = Size * 8,
	<<Head:Index/binary, _OldValue:Size/binary, Tail/binary>> = Values,
	if Type == int ->
			<<Head:Index/binary, Value:BitSize/signed-little-integer, Tail/binary>>;
		Type == uint ->
			<<Head:Index/binary, Value:BitSize/unsigned-little-integer, Tail/binary>>
	end.
