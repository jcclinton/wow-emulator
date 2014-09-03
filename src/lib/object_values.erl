%%   This is a World of Warcraft emulator written in erlang, supporting
%%   client 1.12.x
%%
%%   Copyright (C) 2014  Jamie Clinton <jamieclinton.com>
%%
%%   This program is free software; you can redistribute it and/or modify
%%   it under the terms of the GNU General Public License as published by
%%   the Free Software Foundation; either version 2 of the License, or
%%   (at your option) any later version.
%%
%%   This program is distributed in the hope that it will be useful,
%%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%   GNU General Public License for more details.
%%
%%   You should have received a copy of the GNU General Public License along
%%   with this program; if not, write to the Free Software Foundation, Inc.,
%%   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%
%%   World of Warcraft, and all World of Warcraft or Warcraft art, images,
%%   and lore ande copyrighted by Blizzard Entertainment, Inc.

-module(object_values).

-export([get_byte_value/3,
				get_uint16_value/3,
				get_uint32_value/2,
				get_int32_value/2,
				get_uint64_value/2,
				get_float_value/2
]).
-export([set_byte_value/4,
				set_uint16_value/4,
				set_uint32_value/3,
				set_int32_value/3,
				set_uint64_value/3,
				set_float_value/3
]).
-export([set_value/2, get_value/2]).

-include("include/binary.hrl").


set_value({FieldData, Value, Type}, Values) ->
	{Field, Offset} = if is_tuple(FieldData) -> FieldData;
		is_atom(FieldData) -> {FieldData, 0}
	end,
	% for 32 bit words, offset is the word offset
	% for 8 and 16 bit words, offset is the byte offset
	Index = object_fields:fields(Field) + Offset,
	case Type of
		uint32 ->
			set_uint32_value(Index, Value, Values);
		uint64 ->
			set_uint64_value(Index, Value, Values);
		float ->
			set_float_value(Index, Value, Values);
		uint16 ->
			set_uint16_value(Field, Value, Values, Offset);
		uint8 ->
			set_byte_value(Field, Value, Values, Offset);
		int32 ->
			set_int32_value(Index, Value, Values)
	end.

get_value({FieldData, Type}, Values) ->
	{Field, Offset} = if is_tuple(FieldData) -> FieldData;
		is_atom(FieldData) -> {FieldData, 0}
	end,
	Index = object_fields:fields(Field) + Offset,
	case Type of
		uint32 ->
			get_uint32_value(Index, Values);
		uint64 ->
			get_uint64_value(Index, Values);
		float ->
			get_float_value(Index, Values);
		uint16 ->
			get_uint16_value(Field, Values, Offset);
		uint8 ->
			get_byte_value(Field, Values, Offset);
		int32 ->
			get_int32_value(Index, Values)
	end.


get_byte_value(Field, Values, Offset) ->
	get_value(Field, Values, 1, Offset, uint).

get_uint16_value(Field, Values, Offset) ->
	get_value(Field, Values, 2, Offset, uint).

get_int32_value(Field, Values) ->
	get_value(Field, Values, 4, 0, int).

get_uint32_value(Field, Values) ->
	get_value(Field, Values, 4, 0, uint).

get_uint64_value(Field, Values) ->
	get_value(Field, Values, 8, 0, uint).

get_float_value(Field, Values) ->
	get_value(Field, Values, 4, 0, float).


get_value(Field, Values, Size, Offset, Type) when is_atom(Field) ->
	Index = object_fields:fields(Field),
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
			Value;
		Type == float ->
			<<_Head:Index/binary, Value?f, _Tail/binary>> = Values,
			Value
	end.





set_byte_value(Field, Value, Values, Offset) ->
	if Offset > 3 orelse Offset < 0 -> throw(badarg);
		true -> ok
	end,
	if Value > 16#FF orelse Value < 0 -> throw(badarg);
		true -> ok
	end,
	set_value(Field, Value, Values, 1, Offset, uint).

set_uint16_value(Field, Value, Values, Offset) ->
	if Offset > 1 orelse Offset < 0 -> throw(badarg);
		true -> ok
	end,
	if Value > 16#FFFF orelse Value < 0 -> throw(badarg);
		true -> ok
	end,
	set_value(Field, Value, Values, 2, Offset, uint).

set_int32_value(Field, Value, Values) ->
	if Value > 16#FFFFFFFF -> throw(badarg);
		true -> ok
	end,
	set_value(Field, Value, Values, 4, 0, int).

set_uint32_value(Field, Value, Values) ->
	if Value > 16#FFFFFFFF orelse Value < 0 -> throw(badarg);
		true -> ok
	end,
	set_value(Field, Value, Values, 4, 0, uint).

set_uint64_value(Field, Value, Values) ->
	if Value > 16#FFFFFFFFFFFFFFFF orelse Value < 0 -> throw(badarg);
		true -> ok
	end,
	set_value(Field, Value, Values, 8, 0, uint).

set_float_value(Field, Value, Values) ->
	set_value(Field, Value, Values, 4, 0, float).




set_value(Field, Value, Values, Size, Offset, Type) when is_atom(Field) ->
	Index = object_fields:fields(Field),
	set_value(Index, Value, Values, Size, Offset, Type);
set_value(IndexIn, Value, Values, Size, Offset, Type) ->
	% each Index is a 4 byte long word
	Index =  4 * IndexIn + Offset,
	BitSize = Size * 8,
	<<Head:Index/binary, _OldValue:Size/binary, Tail/binary>> = Values,
	if Type == int ->
			<<Head:Index/binary, Value:BitSize/signed-little-integer, Tail/binary>>;
		Type == uint ->
			<<Head:Index/binary, Value:BitSize/unsigned-little-integer, Tail/binary>>;
		Type == float ->
			<<Head:Index/binary, Value?f, Tail/binary>>
	end.
