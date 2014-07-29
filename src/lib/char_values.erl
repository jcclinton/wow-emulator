-module(char_values).


-export([set_anim_state/2, set_sheathed/2]).
-export([set_item/3, set_item/4]).
-export([set_visible_item/3, set_visible_item/4]).
-export([get/2]).
-compile([export_all]). % needed to call functions through get/1


-include("include/items.hrl").


% sets

set_sheathed(Value, Values) ->
	Field = 'UNIT_FIELD_BYTES_2',
	Offset = 0,
	set_byte_mark_if_needed(Field, Value, Values, Offset).


% sitting 1
% standing 0
set_anim_state(AnimState, Values) ->
	Field = 'UNIT_FIELD_BYTES_1',
	Offset = 0,
	set_byte_mark_if_needed(Field, AnimState, Values, Offset).



set_item(Slot, ItemGuid, Values) ->
	set_item(Slot, ItemGuid, Values, true).
set_item(Slot, ItemGuid, Values, MarkUpdate) ->
	Field = 'PLAYER_FIELD_INV_SLOT_HEAD',
	Index = update_fields:fields(Field) + (2 * Slot),
	set_uint64_mark_if_needed(Index, ItemGuid, Values, MarkUpdate).


set_visible_item(Slot, ItemId, Values) ->
	set_visible_item(Slot, ItemId, Values, true).
set_visible_item(Slot, ItemId, Values, MarkUpdate) ->
	Field = 'PLAYER_VISIBLE_ITEM_1_0',
	Index = update_fields:fields(Field) + (Slot * ?max_visible_item_offset),
	set_uint32_mark_if_needed(Index, ItemId, Values, MarkUpdate).




% private helpers

set_uint32_mark_if_needed(Field, NewValue, Values) ->
	set_uint32_mark_if_needed(Field, NewValue, Values, true).
set_uint32_mark_if_needed(Field, NewValue, Values, MarkUpdate) ->
	Value = object_values:get_uint32_value(Field, Values),
	if Value /= NewValue ->
			if MarkUpdate -> mark_update(Field, Values);
				true -> ok
			end,
			object_values:set_uint32_value(Field, NewValue, Values);
		true -> Values
	end.


set_uint64_mark_if_needed(Field, NewValue, Values) ->
	set_uint64_mark_if_needed(Field, NewValue, Values, true).
set_uint64_mark_if_needed(Field, NewValue, Values, MarkUpdate) ->
	Value = object_values:get_uint64_value(Field, Values),
	if Value /= NewValue ->
			if MarkUpdate -> mark_update(Field, Values); %todo mark both 32 bit flags
				true -> ok
			end,
			object_values:set_uint64_value(Field, NewValue, Values);
		true -> Values
	end.


set_byte_mark_if_needed(Field, NewValue, Values, Offset) ->
	set_byte_mark_if_needed(Field, NewValue, Values, Offset, true).
set_byte_mark_if_needed(Field, NewValue, Values, Offset, MarkUpdate) ->
	Value = object_values:get_byte_value(Field, Values, Offset),
	if Value /= NewValue ->
			if MarkUpdate -> mark_update(Field, Values);
				true -> ok
			end,
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
			0
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

%returns guid of item in a given slot
item(Slot, Values) ->
	Field = 'PLAYER_FIELD_INV_SLOT_HEAD',
	Index = update_fields:fields(Field) + (2 * Slot),
	object_values:get_uint64_value(Index, Values).




%% private

mark_update(Field, Values) ->
	Guid = get(guid, Values),
	Mask = char_data:get_mask(Guid),
	NewMask = update_mask:set_bit(Field, Mask),
	char_data:store_mask(Guid, NewMask),
	ok.
	%use key to update bit mask
	% set update to run every 30ms or so, if its been marked
