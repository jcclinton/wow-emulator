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
-include("include/data_types.hrl").


% gets

get_guid(Values) ->
	object_values:get_uint64_value(object_field_guid, Values).

get_item_id(Values) ->
	object_values:get_uint32_value(object_field_entry, Values).

get_stack_count(Values) ->
	object_values:get_uint32_value(item_field_stack_count, Values).

get_owner(Values) ->
	object_values:get_uint64_value(item_field_owner, Values).

get_contained(Values) ->
	object_values:get_uint64_value(item_field_contained, Values).


% sets

set_stack_count(Value, Values) ->
	object_values:set_uint32_value(item_field_stack_count, Value, Values).

set_contained(Value, Values) ->
	object_values:set_uint64_value(item_field_contained, Value, Values).

set_guid(Value, Values) ->
	object_values:set_uint64_value(object_field_guid, Value, Values).

set_item_id(Value, Values) ->
	object_values:set_uint32_value(object_field_entry, Value, Values).

set_owner(Value, Values) ->
	object_values:set_uint64_value(item_field_owner, Value, Values).



% create values

-spec create(guid(), non_neg_integer(), guid()) -> item_values().
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
		{object_field_guid, ItemGuid, uint64},
		{object_field_type, ObjectType, uint32},
    {object_field_scale_x, Scale, float},
    {object_field_entry, ItemId, uint32},

    {item_field_spell_charges, Charges, int32},
    {item_field_spell_charges_01, Charges, int32},
    {item_field_spell_charges_02, Charges, int32},
    {item_field_spell_charges_03, Charges, int32},
    {item_field_spell_charges_04, Charges, int32},
    {item_field_owner, OwnerGuid, uint64},
    {item_field_contained, ItemGuid, uint64},
    {item_field_stack_count, StackCount, uint32},
    {item_field_maxdurability, ItemMaxDurability, uint32},
    {item_field_durability, ItemMaxDurability, uint32}
	],

	EmptyValues = get_empty_values(),
	lists:foldl(fun object_values:set_value/2, EmptyValues, KeyValues).


-spec get_empty_values() -> item_values().
get_empty_values() ->
	TotalCount = object_fields:get_total_count(item),
	binary:copy(<<0?L>>, TotalCount).
