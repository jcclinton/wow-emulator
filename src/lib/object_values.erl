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
				get_float_value/2,
				get_value/2]).
-export([set_byte_value/4,
				set_uint16_value/4,
				set_uint32_value/3,
				set_int32_value/3,
				set_uint64_value/3,
				set_float_value/3
				]).
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
		{uint16, Offset} ->
			set_uint16_value(IndexName, Value, Values, Offset);
		{byte, Offset} ->
			set_byte_value(IndexName, Value, Values, Offset);
		float ->
			set_float_value(IndexName, Value, Values);
		{float, Offset} ->
			Index = update_fields:fields(IndexName) + Offset,
			set_float_value(Index, Value, Values)
	end.


get_byte_value(IndexName, Values, Offset) ->
	get_value(IndexName, Values, 1, Offset).

get_uint16_value(IndexName, Values, Offset) ->
	get_value(IndexName, Values, 2, Offset).

get_int32_value(IndexName, Values) ->
	get_value(IndexName, Values, 4, 0, int).

get_value(Index, Values) ->
	get_uint32_value(Index, Values).

get_uint32_value(IndexName, Values) ->
	get_value(IndexName, Values, 4, 0).

get_uint64_value(IndexName, Values) ->
	get_value(IndexName, Values, 8, 0).

get_float_value(IndexName, Values) ->
	get_value(IndexName, Values, 4, 0, float).


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
			Value;
		Type == float ->
			<<_Head:Index/binary, Value?f, _Tail/binary>> = Values,
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

set_uint64_value(IndexName, Value, Values) ->
	if Value > 16#FFFFFFFFFFFFFFFF orelse Value < 0 -> throw(badarg);
		true -> ok
	end,
	set_value(IndexName, Value, Values, 8, 0).

set_float_value(IndexName, Value, Values) ->
	set_value(IndexName, Value, Values, 4, 0, float).




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
			<<Head:Index/binary, Value:BitSize/unsigned-little-integer, Tail/binary>>;
		Type == float ->
			<<Head:Index/binary, Value?f, Tail/binary>>
	end.
