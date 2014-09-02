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


-export([get/2, set/3]).
-export([get_empty_values/0]).
-compile([export_all]). % needed to call functions through get/1


-include("include/items.hrl").
-include("include/binary.hrl").

get_empty_values() ->
	TotalCount = object_fields:get_total_count(player),
	% create initially empty binary values object
	binary:copy(<<0?L>>, TotalCount).


% sets

set(FieldName, Value, Values) ->
	try ?MODULE:FieldName(Value, Values) of
		Result -> Result
	catch
		Error ->
			io:format("ERROR trying to set ~p on function name ~p for char_value: ~p~n", [FieldName, Value, Error]),
			{Values, []}
	end.


armor(Value, Values) when not is_integer(Value) ->
	NewValue = round(Value),
	armor(NewValue, Values);
armor(Value, Values) ->
	Field = unit_field_resistances,
	Index = object_fields:fields(Field),
	set_uint32_mark_if_needed(Index, Value, Values).

health(Value, Values) ->
	Field = unit_field_health,
	Index = object_fields:fields(Field),
	set_uint32_mark_if_needed(Index, Value, Values).

block(Value, Values) ->
	Field = player_block_percentage,
	Index = object_fields:fields(Field),
	set_uint32_mark_if_needed(Index, Value, Values).

base_attack_time(Value, Values) when not is_float(Value) ->
	NewValue = float(Value),
	base_attack_time(NewValue, Values);
base_attack_time(Value, Values) ->
	Field = unit_field_baseattacktime,
	Index = object_fields:fields(Field),
	set_float_mark_if_needed(Index, Value, Values).

min_damage(Value, Values) when not is_float(Value) ->
	NewValue = float(Value),
	min_damage(NewValue, Values);
min_damage(Value, Values) ->
	Field = unit_field_mindamage,
	Index = object_fields:fields(Field),
	set_float_mark_if_needed(Index, Value, Values).

max_damage(Value, Values) when not is_float(Value) ->
	NewValue = float(Value),
	max_damage(NewValue, Values);
max_damage(Value, Values) ->
	Field = unit_field_maxdamage,
	Index = object_fields:fields(Field),
	set_float_mark_if_needed(Index, Value, Values).

% sitting 1
% standing 0
anim_state(AnimState, Values) ->
	Field = unit_field_bytes_1,
	Offset = 0,
	set_byte_mark_if_needed(Field, AnimState, Values, Offset).

sheathed(Value, Values) ->
	Field = unit_field_bytes_2,
	Offset = 0,
	set_byte_mark_if_needed(Field, Value, Values, Offset).

target(Value, Values) ->
	Field = unit_field_target,
	set_uint64_mark_if_needed(Field, Value, Values).






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
get(FuncName, Values) ->
	try ?MODULE:FuncName(Values) of
		Val -> Val
	catch
		Error ->
			io:format("ERROR trying to get char_value: ~p~n", [Error]),
			Values
	end.


% private get functions

guid(Values) ->
	object_values:get_uint64_value( object_field_guid, Values).

guild_id(Values) ->
	object_values:get_uint32_value(player_guildid, Values).

level(Values) ->
	object_values:get_uint32_value(unit_field_level, Values).

skin(Values) ->
	object_values:get_byte_value(player_bytes, Values, 0).

face(Values) ->
	object_values:get_byte_value(player_bytes, Values, 1).

hair_style(Values) ->
	object_values:get_byte_value(player_bytes, Values, 2).

hair_color(Values) ->
	object_values:get_byte_value(player_bytes, Values, 3).

facial_hair(Values) ->
	object_values:get_byte_value(player_bytes_2, Values, 0).

race(Values) ->
	object_values:get_byte_value(unit_field_bytes_0, Values, 0).

class(Values) ->
	object_values:get_byte_value(unit_field_bytes_0, Values, 1).

gender(Values) ->
	object_values:get_byte_value(unit_field_bytes_0, Values, 2).

armor(Values) ->
	object_values:get_uint32_value(unit_field_resistances, Values).

base_attack_time(Values) ->
	object_values:get_float_value(unit_field_baseattacktime, Values).

max_damage(Values) ->
	object_values:get_float_value(unit_field_maxdamage, Values).

min_damage(Values) ->
	object_values:get_float_value(unit_field_mindamage, Values).

anim_state(Values) ->
	object_values:get_byte_value(unit_field_bytes_1, Values, 0).

mod_strength(Values) ->
	object_values:get_float_value(player_field_posstat0, Values).

mod_agility(Values) ->
	object_values:get_float_value(player_field_posstat1, Values).

mod_stamina(Values) ->
	object_values:get_float_value(player_field_posstat2, Values).

mod_intellect(Values) ->
	object_values:get_float_value(player_field_posstat3, Values).

mod_spirit(Values) ->
	object_values:get_float_value(player_field_posstat4, Values).

health(Values) ->
	object_values:get_uint32_value(unit_field_health, Values).

max_health(Values) ->
	object_values:get_uint32_value(unit_field_maxhealth, Values).

target(Values) ->
	object_values:get_uint64_value(unit_field_target, Values).


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


