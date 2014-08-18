-module(char_values).


-export([set_item/3, set_visible_item/3]).
-export([set_aura/3, set_aura_level/3, set_aura_application/2, set_aura_flag/2]).
-export([get/2, set/3]).
-export([get_empty_values/0]).
-compile([export_all]). % needed to call functions through get/1


-include("include/items.hrl").

get_empty_values() ->
	TotalCount = update_fields:get_total_count(player),
	% create initially empty binary values object
	binary:copy(<<0?L>>, TotalCount).


% sets

set(Type, Value, Input) ->
	Values = if is_binary(Input) -> Input;
		is_number(Input) ->
			% input is the guid, lookup the values object
			char_data:get_values(Input)
	end,
	try ?MODULE:Type(Value, Values) of
		Val -> Val
	catch
		Error ->
			io:format("ERROR trying to set ~p on type ~p for char_value: ~p~n", [Type, Value, Error]),
			Values
	end.


armor(Value, Values) when not is_integer(Value) ->
	NewValue = round(Value),
	armor(NewValue, Values);
armor(Value, Values) ->
	Field = 'UNIT_FIELD_RESISTANCES',
	Index = update_fields:fields(Field),
	set_uint32_mark_if_needed(Index, Value, Values).

health(Value, Values) ->
	Field = 'UNIT_FIELD_HEALTH',
	Index = update_fields:fields(Field),
	set_uint32_mark_if_needed(Index, Value, Values).

block(Value, Values) ->
	Field = 'PLAYER_BLOCK_PERCENTAGE',
	Index = update_fields:fields(Field),
	set_uint32_mark_if_needed(Index, Value, Values).

base_attack_time(Value, Values) when not is_float(Value) ->
	NewValue = float(Value),
	base_attack_time(NewValue, Values);
base_attack_time(Value, Values) ->
	Field = 'UNIT_FIELD_BASEATTACKTIME',
	Index = update_fields:fields(Field),
	set_float_mark_if_needed(Index, Value, Values).

min_damage(Value, Values) when not is_float(Value) ->
	NewValue = float(Value),
	min_damage(NewValue, Values);
min_damage(Value, Values) ->
	Field = 'UNIT_FIELD_MINDAMAGE',
	Index = update_fields:fields(Field),
	set_float_mark_if_needed(Index, Value, Values).

max_damage(Value, Values) when not is_float(Value) ->
	NewValue = float(Value),
	max_damage(NewValue, Values);
max_damage(Value, Values) ->
	Field = 'UNIT_FIELD_MAXDAMAGE',
	Index = update_fields:fields(Field),
	set_float_mark_if_needed(Index, Value, Values).

anim_state(AnimState, Values) ->
	Field = 'UNIT_FIELD_BYTES_1',
	Offset = 0,
	set_byte_mark_if_needed(Field, AnimState, Values, Offset).

sheathed(Value, Values) ->
	Field = 'UNIT_FIELD_BYTES_2',
	Offset = 0,
	set_byte_mark_if_needed(Field, Value, Values, Offset).







% sitting 1
% standing 0

set_aura(Slot, SpellId, Values) ->
	Field = 'UNIT_FIELD_AURA',
	Index = update_fields:fields(Field) + Slot,
	NextIndex = update_fields:fields('UNIT_FIELD_AURA_LAST'),
	if Index >= NextIndex orelse Slot < 0 -> throw(badarg);
		true -> ok
	end,
	set_uint32_mark_if_needed(Index, SpellId, Values).

set_aura_flag(Slot, Values) ->
	SlotIndex = Slot bsr 3,
	Field = 'UNIT_FIELD_AURAFLAGS',
	Index = update_fields:fields(Field) + SlotIndex,
	Flags = object_values:get_uint32_value(Index, Values),
	Byte = (Slot band 7) bsl 2,
	FlagMask = 9,
	NewFlags = Flags bor (FlagMask bsl Byte),
	set_uint32_mark_if_needed(Index, NewFlags, Values).

set_aura_level(Slot, Level, Values) ->
	SlotIndex = Slot div 4,
	Byte = (Slot rem 4) * 8,
	Field = 'UNIT_FIELD_AURALEVELS',
	Index = update_fields:fields(Field) + SlotIndex,
	OldLevels = object_values:get_uint32_value(Index, Values),

	Tmp = OldLevels band (bnot (16#FF bsl Byte)),
	NewLevels = Tmp bor (Level bsl Byte),

	set_uint32_mark_if_needed(Index, NewLevels, Values).

set_aura_application(Slot, Values) ->
	SlotIndex = Slot div 4,
	Byte = (Slot rem 4) * 8,
	Field = 'UNIT_FIELD_AURAAPPLICATIONS',
	Index = update_fields:fields(Field) + SlotIndex,
	OldApp = object_values:get_uint32_value(Index, Values),

	NewApp = OldApp bor (0 bsl Byte),

	set_uint32_mark_if_needed(Index, NewApp, Values).





set_item(Slot, ItemGuid, Values) ->
	Field = 'PLAYER_FIELD_INV_SLOT_HEAD',
	Index = update_fields:fields(Field) + (2 * Slot),
	% this can set an item from equipped, bags, bag inventory, bank and keyrings
	NextIndex = update_fields:fields('PLAYER_FARSIGHT'),
	if Index >= NextIndex orelse Slot < 0 -> throw(badarg);
		true -> ok
	end,
	set_uint64_mark_if_needed(Index, ItemGuid, Values).


set_visible_item(Slot, ItemId, Values) ->
	Field = 'PLAYER_VISIBLE_ITEM_1_0',
	Index = update_fields:fields(Field) + (Slot * ?max_visible_item_offset),
	NextIndex = update_fields:fields('PLAYER_FIELD_INV_SLOT_HEAD'),
	% this can only be used to set equipped items
	if Index >= NextIndex orelse Slot < 0 -> throw(badarg);
		true -> ok
	end,
	set_uint32_mark_if_needed(Index, ItemId, Values).




% private helpers

set_float_mark_if_needed(Field, NewValue, Values) ->
	Value = object_values:get_float_value(Field, Values),
	if Value /= NewValue ->
			mark_update(Field, Values, 32),
			object_values:set_float_value(Field, NewValue, Values);
		true -> Values
	end.

set_uint32_mark_if_needed(Field, NewValue, Values) ->
	Value = object_values:get_uint32_value(Field, Values),
	if Value /= NewValue ->
			mark_update(Field, Values, 32),
			object_values:set_uint32_value(Field, NewValue, Values);
		true -> Values
	end.


set_uint64_mark_if_needed(Field, NewValue, Values) ->
	Value = object_values:get_uint64_value(Field, Values),
	if Value /= NewValue ->
			mark_update(Field, Values, 64),
			object_values:set_uint64_value(Field, NewValue, Values);
		true -> Values
	end.


set_byte_mark_if_needed(Field, NewValue, Values, Offset) ->
	Value = object_values:get_byte_value(Field, Values, Offset),
	if Value /= NewValue ->
			mark_update(Field, Values, 32),
			object_values:set_byte_value(Field, NewValue, Values, Offset);
		true -> Values
	end.




%% gets
get(Type, Input) ->
	Values = if is_binary(Input) -> Input;
		is_number(Input) ->
			% input is the guid, lookup the values object
			char_data:get_values(Input)
	end,
	try ?MODULE:Type(Values) of
		Val -> Val
	catch
		Error ->
			io:format("ERROR trying to get char_value: ~p~n", [Error]),
			Values
	end.


% private get functions

guid(Values) ->
	object_values:get_uint64_value( 'OBJECT_FIELD_GUID', Values).

guild_id(Values) ->
	object_values:get_uint32_value('PLAYER_GUILDID', Values).

level(Values) ->
	object_values:get_uint32_value('UNIT_FIELD_LEVEL', Values).

skin(Values) ->
	object_values:get_byte_value('PLAYER_BYTES', Values, 0).

face(Values) ->
	object_values:get_byte_value('PLAYER_BYTES', Values, 1).

hair_style(Values) ->
	object_values:get_byte_value('PLAYER_BYTES', Values, 2).

hair_color(Values) ->
	object_values:get_byte_value('PLAYER_BYTES', Values, 3).

facial_hair(Values) ->
	object_values:get_byte_value('PLAYER_BYTES_2', Values, 0).

race(Values) ->
	object_values:get_byte_value('UNIT_FIELD_BYTES_0', Values, 0).

class(Values) ->
	object_values:get_byte_value('UNIT_FIELD_BYTES_0', Values, 1).

gender(Values) ->
	object_values:get_byte_value('UNIT_FIELD_BYTES_0', Values, 2).

armor(Values) ->
	object_values:get_uint32_value('UNIT_FIELD_RESISTANCES', Values).

base_attack_time(Values) ->
	object_values:get_float_value('UNIT_FIELD_BASEATTACKTIME', Values).

max_damage(Values) ->
	object_values:get_float_value('UNIT_FIELD_MAXDAMAGE', Values).

min_damage(Values) ->
	object_values:get_float_value('UNIT_FIELD_MINDAMAGE', Values).

anim_state(Values) ->
	object_values:get_byte_value('UNIT_FIELD_BYTES_1', Values, 0).

mod_strength(Values) ->
	object_values:get_float_value('PLAYER_FIELD_POSSTAT0', Values).

mod_agility(Values) ->
	object_values:get_float_value('PLAYER_FIELD_POSSTAT1', Values).

mod_stamina(Values) ->
	object_values:get_float_value('PLAYER_FIELD_POSSTAT2', Values).

mod_intellect(Values) ->
	object_values:get_float_value('PLAYER_FIELD_POSSTAT3', Values).

mod_spirit(Values) ->
	object_values:get_float_value('PLAYER_FIELD_POSSTAT4', Values).

health(Values) ->
	object_values:get_uint32_value('UNIT_FIELD_HEALTH', Values).

max_health(Values) ->
	object_values:get_uint32_value('UNIT_FIELD_MAXHEALTH', Values).


%returns guid of item in a given slot
item(Slot, Values) ->
	Field = 'PLAYER_FIELD_INV_SLOT_HEAD',
	Index = update_fields:fields(Field) + (2 * Slot),
	object_values:get_uint64_value(Index, Values).




%% private

mark_update(Field, Values, 32) when is_atom(Field) ->
	Index = update_fields:fields(Field),
	mark_update(Index, Values, 32);
mark_update(Index, Values, 32) ->
	mark_update([Index], Values);
mark_update(Field, Values, 64) when is_atom(Field) ->
	Index = update_fields:fields(Field),
	mark_update(Index, Values, 64);
mark_update(Index, Values, 64) ->
	mark_update([Index, Index + 1], Values).

mark_update(Indices, Values) when is_list(Indices) ->
	Guid = get(guid, Values),
	char_sess:mark_update(Guid, Indices).
