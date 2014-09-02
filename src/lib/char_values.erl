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


-export([get/2, set/3, set_value/3]).
-export([get_empty_values/0]).
-compile([export_all]). % needed to call functions through get/1


-include("include/items.hrl").
-include("include/binary.hrl").

get_empty_values() ->
	TotalCount = object_fields:get_total_count(player),
	% create initially empty binary values object
	binary:copy(<<0?L>>, TotalCount).


% sets

set_value(FieldData, Value, Values) ->
	{Field, Offset} = if is_tuple(FieldData) -> FieldData;
		is_atom(FieldData) -> {FieldData, 0}
	end,
	% TODO check if the value really changed, if not do nothing here
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
	{NewValues, Indices}.


set(FieldName, Value, Values) ->
	try ?MODULE:FieldName(Value, Values) of
		Result -> Result
	catch
		Error ->
			io:format("ERROR trying to set ~p on function name ~p for char_value: ~p~n", [FieldName, Value, Error]),
			{Values, []}
	end.







aura({Slot, SpellId}, Values) ->
	Field = unit_field_aura,
	Index = object_fields:fields(Field) + Slot,
	NextIndex = object_fields:fields(unit_field_aura_last),
	if Index >= NextIndex orelse Slot < 0 -> throw(badarg);
		true -> ok
	end,
	set_uint32_mark_if_needed(Index, SpellId, Values).

aura_flag(Slot, Values) ->
	SlotIndex = Slot bsr 3,
	Field = unit_field_auraflags,
	Index = object_fields:fields(Field) + SlotIndex,
	Flags = object_values:get_uint32_value(Index, Values),
	Byte = (Slot band 7) bsl 2,
	FlagMask = 9,
	NewFlags = Flags bor (FlagMask bsl Byte),
	set_uint32_mark_if_needed(Index, NewFlags, Values).

aura_level({Slot, Level}, Values) ->
	SlotIndex = Slot div 4,
	Byte = (Slot rem 4) * 8,
	Field = unit_field_auralevels,
	Index = object_fields:fields(Field) + SlotIndex,
	OldLevels = object_values:get_uint32_value(Index, Values),

	Tmp = OldLevels band (bnot (16#FF bsl Byte)),
	NewLevels = Tmp bor (Level bsl Byte),

	set_uint32_mark_if_needed(Index, NewLevels, Values).

aura_application(Slot, Values) ->
	SlotIndex = Slot div 4,
	Byte = (Slot rem 4) * 8,
	Field = unit_field_auraapplications,
	Index = object_fields:fields(Field) + SlotIndex,
	OldApp = object_values:get_uint32_value(Index, Values),

	NewApp = OldApp bor (0 bsl Byte),

	set_uint32_mark_if_needed(Index, NewApp, Values).





item({Slot, ItemGuid}, Values) ->
	Field = player_field_inv_slot_head,
	Index = object_fields:fields(Field) + (2 * Slot),
	% this can set an item from equipped, bags, bag inventory, bank and keyrings
	NextIndex = object_fields:fields(player_farsight),
	if Index >= NextIndex orelse Slot < 0 -> throw(badarg);
		true -> ok
	end,
	set_uint64_mark_if_needed(Index, ItemGuid, Values).


visible_item({Slot, ItemId}, Values) ->
	Field = player_visible_item_1_0,
	Index = object_fields:fields(Field) + (Slot * ?max_visible_item_offset),
	NextIndex = object_fields:fields(player_field_inv_slot_head),
	% this can only be used to set equipped items
	if Index >= NextIndex orelse Slot < 0 -> throw(badarg);
		true -> ok
	end,
	set_uint32_mark_if_needed(Index, ItemId, Values).




%% gets
get_value(FieldData, Values) ->
	Field = case FieldData of
		{FieldName, _} -> FieldName;
		_ -> FieldData
	end,
	Type = object_fields:type(Field),
	object_values:get_value({FieldData, Type}, Values).


get(FuncName, Values) ->
	try ?MODULE:FuncName(Values) of
		Val -> Val
	catch
		Error ->
			io:format("ERROR trying to get char_value: ~p~n", [Error]),
			Values
	end.


% private get functions

%returns guid of item in a given slot
item({Slot, Values}) ->
	Field = player_field_inv_slot_head,
	Index = object_fields:fields(Field) + (2 * Slot),
	object_values:get_uint64_value(Index, Values).




%% private

mark_update(Field, 32) when is_atom(Field) ->
	Index = object_fields:fields(Field),
	mark_update(Index, 32);
mark_update(Index, 32) ->
	[Index];
mark_update(Field, 64) when is_atom(Field) ->
	Index = object_fields:fields(Field),
	mark_update(Index, 64);
mark_update(Index, 64) ->
	[Index, Index + 1].




set_float_mark_if_needed(Field, NewValue, Values) ->
	Value = object_values:get_float_value(Field, Values),
	if Value /= NewValue ->
			Indices = mark_update(Field, 32),
			NewValues = object_values:set_float_value(Field, NewValue, Values),
			{NewValues, Indices};
		true -> {Values, []}
	end.

set_uint32_mark_if_needed(Field, NewValue, Values) ->
	Value = object_values:get_uint32_value(Field, Values),
	if Value /= NewValue ->
			Indices = mark_update(Field, 32),
			NewValues = object_values:set_uint32_value(Field, NewValue, Values),
			{NewValues, Indices};
		true -> {Values, []}
	end.


set_uint64_mark_if_needed(Field, NewValue, Values) ->
	Value = object_values:get_uint64_value(Field, Values),
	if Value /= NewValue ->
			Indices = mark_update(Field, 64),
			NewValues = object_values:set_uint64_value(Field, NewValue, Values),
			{NewValues, Indices};
		true -> {Values, []}
	end.


set_byte_mark_if_needed(Field, NewValue, Values, Offset) ->
	Value = object_values:get_byte_value(Field, Values, Offset),
	if Value /= NewValue ->
			Indices = mark_update(Field, 32),
			NewValues = object_values:set_byte_value(Field, NewValue, Values, Offset),
			{NewValues, Indices};
		true -> {Values, []}
	end.


