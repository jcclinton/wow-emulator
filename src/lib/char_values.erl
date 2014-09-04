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

-module(char_values).


-export([get_value/2, set_value/3]).
-export([get_empty_values/0]).


-include("include/items.hrl").
-include("include/binary.hrl").
-include("include/data_types.hrl").

-spec get_empty_values() -> player_values().
get_empty_values() ->
	TotalCount = object_fields:get_total_count(player),
	% create initially empty binary values object
	binary:copy(<<0?L>>, TotalCount).


% sets

-spec set_value(field_data(), number(), player_values()) -> {player_values(), [non_neg_integer()]}.
set_value(FieldData, Value, Values) ->
	{Field, Offset} = if is_tuple(FieldData) -> FieldData;
		is_atom(FieldData) -> {FieldData, 0}
	end,
	OldValue = get_value(FieldData, Values),
	% check if existing value is the same
	% if it is, do nothing
	if OldValue == Value -> {Values, []};
		OldValue /= Value ->
			{Index, Type} = object_fields:field_data(Field),
			% offsets can be passed in for any type
			% but uint16 and uint8 offsets are byte offsets
			% the rest are word offsets
			OffsetIndex = Index + Offset,
			Indices = case Type of
				uint64 -> [OffsetIndex, OffsetIndex+1];
				uint32 -> [OffsetIndex];
				int32 -> [OffsetIndex];
				float -> [OffsetIndex];
				uint16 -> [Index];
				uint8 -> [Index]
			end,
			NewValues = object_values:set_value({FieldData, Value, Type}, Values),
			{NewValues, Indices}
	end.



%% gets
-spec get_value(field_data(), player_values()) -> number().
get_value(FieldData, Values) ->
	Field = case FieldData of
		{FieldName, _} -> FieldName;
		_ -> FieldData
	end,
	Type = object_fields:type(Field),
	object_values:get_value({FieldData, Type}, Values).
