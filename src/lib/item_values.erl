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

-module(item_values).

-export([get_guid/1, get_item_id/1]).
-export([get_stack_count/1]).
-export([get_owner/1, get_contained/1]).

-export([set_guid/2, set_item_id/2]).
-export([set_contained/2, set_owner/2]).
-export([set_stack_count/2]).

-export([create/3]).


-include("include/types.hrl").
-include("include/binary.hrl").
-include("include/shared_defines.hrl").
-include("include/items.hrl").
-include("include/database_records.hrl").


% gets

get_guid(Values) ->
	object_values:get_uint64_value('OBJECT_FIELD_GUID', Values).

get_item_id(Values) ->
	object_values:get_uint32_value('OBJECT_FIELD_ENTRY', Values).

get_stack_count(Values) ->
	object_values:get_uint32_value( 'ITEM_FIELD_STACK_COUNT', Values).

get_owner(Values) ->
	object_values:get_uint64_value('ITEM_FIELD_OWNER', Values).

get_contained(Values) ->
	object_values:get_uint64_value('ITEM_FIELD_CONTAINED', Values).


% sets

set_stack_count(Value, Values) ->
	object_values:set_uint32_value('ITEM_FIELD_STACK_COUNT', Value, Values).

set_contained(Value, Values) ->
	object_values:set_uint64_value('ITEM_FIELD_CONTAINED', Value, Values).

set_guid(Value, Values) ->
	object_values:set_uint64_value('OBJECT_FIELD_GUID', Value, Values).

set_item_id(Value, Values) ->
	object_values:set_uint32_value('OBJECT_FIELD_ENTRY', Value, Values).

set_owner(Value, Values) ->
	object_values:set_uint64_value('ITEM_FIELD_OWNER', Value, Values).



% create values

create(ItemGuid, ItemId, OwnerGuid) ->
	ObjectType = ?typemask_item bor ?typemask_object,
	Scale = ?default_scale,

	ItemProto = content:lookup_item(ItemId),
	ItemMaxDurability = ItemProto#item_proto.max_durability,
	ItemClass = ItemProto#item_proto.class,

	StackCount = if ItemClass == ?item_class_consumable -> ?default_item_stack_size;
		true -> 1
	end,

	Charges = -1,

	KeyValues = [
		{'OBJECT_FIELD_GUID', ItemGuid, uint64},
		{'OBJECT_FIELD_TYPE', ObjectType, uint32},
    {'OBJECT_FIELD_SCALE_X', Scale, float},
    {'OBJECT_FIELD_ENTRY', ItemId, uint32},

    {'ITEM_FIELD_SPELL_CHARGES', Charges, int32},
    {'ITEM_FIELD_SPELL_CHARGES_01', Charges, int32},
    {'ITEM_FIELD_SPELL_CHARGES_02', Charges, int32},
    {'ITEM_FIELD_SPELL_CHARGES_03', Charges, int32},
    {'ITEM_FIELD_SPELL_CHARGES_04', Charges, int32},
    {'ITEM_FIELD_OWNER', OwnerGuid, uint64},
    {'ITEM_FIELD_CONTAINED', ItemGuid, uint64},
    {'ITEM_FIELD_STACK_COUNT', StackCount, uint32},
    {'ITEM_FIELD_MAXDURABILITY', ItemMaxDurability, uint32},
    {'ITEM_FIELD_DURABILITY', ItemMaxDurability, uint32}
	],

	EmptyValues = get_empty_values(),
	lists:foldl(fun object_values:set_key_values/2, EmptyValues, KeyValues).


get_empty_values() ->
	TotalCount = update_fields:get_total_count(item),
	binary:copy(<<0?L>>, TotalCount).
