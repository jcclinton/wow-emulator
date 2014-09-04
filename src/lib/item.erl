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

-module(item).

-export([swap/3]).
-export([equip_new/3]).
-export([get_item_guids/1, get_equipped_item_guids/1]).
-export([slot_empty/2, get_item_guid_at_slot/2]).
-export([get_slot/1]).
-export([is_equippable/1]).
-export([destroy/3]).

-compile([export_all]).

-include("include/types.hrl").
-include("include/binary.hrl").
-include("include/database_records.hrl").
-include("include/character.hrl").
-include("include/items.hrl").
-include("include/shared_defines.hrl").



%used to add items of itemId to guid's inventory
test() ->
	Guid = 1000,
	ItemId = 44,

	lists:foreach(fun(_) ->
		ItemGuid = world:get_guid(?highguid_item, 0),
		ItemValues = item_values:create(ItemGuid, ItemId, Guid),
		item_data:store_values(ItemValues),

		Slot = get_first_empty_inv_slot(Guid),
		item:equip_slot(ItemGuid, Slot, Guid)
	end, lists:seq(1 ,12)),

	ok.


destroy(SrcSlot, Count, Guid) ->
	SrcEmpty = slot_empty(SrcSlot, Guid),
	if SrcEmpty ->
			{error, ?equip_err_item_not_found};
		Count > 0 ->
			ItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
			ItemValues = item_data:get_values(ItemGuid),
			ItemProto = item_data:get_item_proto(ItemValues),
			if ItemProto#item_proto.stackable > 1 ->
					StackCount = item_values:get_value(item_field_stack_count, ItemValues),
					NewStackCount = StackCount - Count,
					if NewStackCount > 0 ->
							NewItemValues = item_values:set_value(item_field_stack_count, NewStackCount, ItemValues),
							item_data:store_values(NewItemValues),
							update_data:build_create_update_packet_for_items([ItemGuid]);
						NewStackCount =< 0 ->
							destroy_at_slot(SrcSlot, Guid)
					end;
				ItemProto#item_proto.stackable =< 1 ->
					destroy_at_slot(SrcSlot, Guid)
			end;
		Count == 0 ->
			destroy_at_slot(SrcSlot, Guid)
	end.

destroy_at_slot(SrcSlot, Guid) ->
	ItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
	item_data:delete_item(ItemGuid),
	remove(SrcSlot, Guid).



swap(SrcSlot, DestSlot, Guid) ->
	SrcEmpty = slot_empty(SrcSlot, Guid),
	DestEmpty = slot_empty(DestSlot, Guid),
	SrcIsValidSlot = is_valid_slot(SrcSlot),
	DestIsValidSlot = is_valid_slot(DestSlot),

	DestIsInvSlot = is_inv_slot(DestSlot),
	DestIsEquipSlot = is_equip_slot(DestSlot),


	if SrcEmpty ->
			{error, ?equip_err_slot_is_empty};
		SrcSlot == DestSlot ->
			{error, ?equip_err_items_cant_be_swapped};
		not SrcIsValidSlot orelse not DestIsValidSlot ->
			{error, ?equip_err_no_equipment_slot_available};
		DestEmpty ->
			if DestIsInvSlot ->
				store_from_slot(SrcSlot, DestSlot, Guid);
				DestIsEquipSlot ->

					CanEquipSrcAtDest = can_equip_from_slot(SrcSlot, DestSlot, Guid),

					if CanEquipSrcAtDest ->
							equip_from_slot(SrcSlot, DestSlot, Guid);
						not CanEquipSrcAtDest ->
							{error, ?equip_err_item_doesnt_go_to_slot}
					end
			end;

		not DestEmpty ->

			CanSwap = can_swap(SrcSlot, DestSlot, Guid),

			if DestIsInvSlot ->

				CanMerge = can_merge(SrcSlot, DestSlot, Guid),

				if CanMerge ->
						merge(SrcSlot, DestSlot, Guid);
					CanSwap ->
						swap_slots(SrcSlot, DestSlot, Guid);
					not CanSwap ->
						{error, ?equip_err_items_cant_be_swapped}
				end;
				DestIsEquipSlot ->

					if CanSwap ->
							swap_slots(SrcSlot, DestSlot, Guid);
						not CanSwap ->
							{error, ?equip_err_items_cant_be_swapped}
					end
			end

	end.

can_split(SrcSlot, DestSlot, Count, Guid) ->
	DestIsValidSlot = is_valid_slot(DestSlot),
	DestEmpty = slot_empty(DestSlot, Guid),
	DestIsInvSlot = is_inv_slot(DestSlot),

	ValidSlot = (SrcSlot /= DestSlot) and DestIsValidSlot and DestEmpty and DestIsInvSlot,

	SrcItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
	SrcItemValues = item_data:get_values(SrcItemGuid),
	SrcItemProto = item_data:get_item_proto(SrcItemValues),
	SrcStackCount = item_values:get_value(item_field_stack_count, SrcItemValues),
	NewStackCount = SrcStackCount - Count,

	ValidStack = (SrcItemProto#item_proto.stackable > 1) and (NewStackCount > 0) and (Count > 0),

	ValidStack and ValidSlot.


split(SrcSlot, DestSlot, Count, Guid) ->
	SrcItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
	SrcItemValues = item_data:get_values(SrcItemGuid),
	SrcItemProto = item_data:get_item_proto(SrcItemValues),
	SrcStackCount = item_values:get_value(item_field_stack_count, SrcItemValues),
	SrcAmount = SrcStackCount - Count,

	%create new dest item
	DestItemGuid = world:get_guid(?highguid_item, 0),
	ItemId = SrcItemProto#item_proto.id,
	DestItemValues = item_values:create(DestItemGuid, ItemId, Guid),
	NewDestItemValues = item_values:set_value(item_field_stack_count, Count, DestItemValues),
	item_data:store_values(NewDestItemValues),
	% add to users items
	equip_slot(DestItemGuid, DestSlot, Guid),

	NewSrcItemValues = item_values:set_value(item_field_stack_count, SrcAmount, SrcItemValues),
	item_data:store_values(NewSrcItemValues),

	update_data:build_create_update_packet_for_items([SrcItemGuid, DestItemGuid]).


can_merge(SrcSlot, DestSlot, Guid) ->
	SrcItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
	DestItemGuid = get_item_guid_at_slot(DestSlot, Guid),

	SrcItemProto = item_data:get_item_proto(SrcItemGuid),
	DestItemProto = item_data:get_item_proto(DestItemGuid),

	% check if they are the same item and that item is stackable
	StackSize = SrcItemProto#item_proto.stackable,
	SrcItemProto#item_proto.id == DestItemProto#item_proto.id andalso StackSize > 1.


merge(SrcSlot, DestSlot, Guid) ->
	SrcItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
	DestItemGuid = get_item_guid_at_slot(DestSlot, Guid),

	SrcItemValues = item_data:get_values(SrcItemGuid),
	DestItemValues = item_data:get_values(DestItemGuid),

	SrcItemProto = item_data:get_item_proto(SrcItemValues),
	StackSize = SrcItemProto#item_proto.stackable,

	SrcStackCount = item_values:get_value(item_field_stack_count, SrcItemValues),
	DestStackCount = item_values:get_value(item_field_stack_count, DestItemValues),
	TotalAmount = SrcStackCount + DestStackCount,

	{DestAmount, SrcAmount} = if TotalAmount > StackSize ->
			{StackSize, TotalAmount - StackSize};
		TotalAmount =< StackSize ->
			{TotalAmount, 0}
	end,

	NewDestItemValues = item_values:set_value(item_field_stack_count, DestAmount, DestItemValues),
	item_data:store_values(NewDestItemValues),

	NewSrcItemValues = item_values:set_value(item_field_stack_count, SrcAmount, SrcItemValues),
	item_data:store_values(NewSrcItemValues),
	if SrcAmount == 0 ->
			%todo how to delete an item from ets and update client?
			remove(SrcSlot, Guid);
		true -> ok
	end,

	update_data:build_create_update_packet_for_items([SrcItemGuid, DestItemGuid]).



can_swap(SrcSlot, DestSlot, Guid) ->
	DestIsInvSlot = is_inv_slot(DestSlot),
	DestIsEquipSlot = is_equip_slot(DestSlot),

	SrcIsInvSlot = is_inv_slot(SrcSlot),
	SrcIsEquipSlot = is_equip_slot(SrcSlot),

	CanEquipSrcAtDest = can_equip_from_slot(SrcSlot, DestSlot, Guid),
	CanEquipDestAtSrc = can_equip_from_slot(DestSlot, SrcSlot, Guid),

	% and has higher precedence than or
	% no point in using short circuit operators here
	DestIsInvSlot and SrcIsInvSlot or
	DestIsInvSlot and SrcIsEquipSlot and CanEquipDestAtSrc or
	DestIsEquipSlot and SrcIsInvSlot and CanEquipSrcAtDest or
	DestIsEquipSlot and SrcIsEquipSlot and CanEquipSrcAtDest and CanEquipDestAtSrc.



% internal function to swap slots
% does not handle inventory checking
swap_slots(SrcSlot, DestSlot, Guid) ->
	SrcItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
	DestItemGuid = get_item_guid_at_slot(DestSlot, Guid),

	DestIsInvSlot = is_inv_slot(DestSlot),
	DestIsEquipSlot = is_equip_slot(DestSlot),

	SrcIsInvSlot = is_inv_slot(SrcSlot),
	SrcIsEquipSlot = is_equip_slot(SrcSlot),

	remove(SrcSlot, Guid),
	remove(DestSlot, Guid),

	equip_slot(SrcItemGuid, DestSlot, Guid),
	if DestIsEquipSlot ->
			visualize_item(Guid, SrcItemGuid, DestSlot);
		DestIsInvSlot -> ok
	end,

	equip_slot(DestItemGuid, SrcSlot, Guid),
	if SrcIsEquipSlot ->
			visualize_item(Guid, DestItemGuid, SrcSlot);
		SrcIsInvSlot -> ok
	end,

	update_data:build_create_update_packet_for_items([SrcItemGuid, DestItemGuid]).


% remove from old slot
% store in inventory
store_from_slot(SrcSlot, DestSlot, Guid) ->
	ItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
	remove(SrcSlot, Guid),
	equip(ItemGuid, DestSlot, Guid).

% remove from old slot
% equip item
equip_from_slot(SrcSlot, DestSlot, Guid) ->
	ItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
	remove(SrcSlot, Guid),
	visualize_item(Guid, ItemGuid, DestSlot),
	equip(ItemGuid, DestSlot, Guid).


can_equip_from_slot(SrcSlot, DestSlot, Guid) ->
	ItemGuid = get_item_guid_at_slot(SrcSlot, Guid),
	can_equip(ItemGuid, DestSlot).

can_equip(ItemGuid, Slot) ->
	ItemProto = item_data:get_item_proto(ItemGuid),
	InvType = ItemProto#item_proto.inventory_type,
	EquipSlot = get_slot(InvType),
	Slot == EquipSlot.



remove(Slot, OwnerGuid) ->
	%ItemGuid = get_item_guid_at_slot(Slot, OwnerGuid),

	player_state:run_async_function(OwnerGuid, set_item, [{Slot, 0}]),

	set_visual_item_slot(OwnerGuid, 0, Slot),
	%stats:update_all(OwnerGuid),
	ok.


is_valid_slot(Slot) ->
	is_inv_slot(Slot) orelse is_equip_slot(Slot).

% inside bag
is_inv_slot(Slot) ->
	Slot >= ?inventory_slot_item_start andalso Slot < ?inventory_slot_item_end.

is_equip_slot(Slot) ->
	Slot >= ?equipment_slot_start andalso Slot < ?equipment_slot_end.


slot_empty(Slot, Guid) ->
	SlotGuid = get_item_guid_at_slot(Slot, Guid),
	SlotGuid == 0.


get_item_guid_at_slot(Slot, Guid) ->
	player_state:run_sync_function(Guid, get_item_guid, [Slot]).







% get just equipped item guids
get_equipped_item_guids(Guid) when is_number(Guid) ->
	ItemGuids = get_item_guids(Guid),
	{Guids, _} = lists:split(?equipment_slot_end, ItemGuids),
	Guids;
get_equipped_item_guids(SlotValues) when is_binary(SlotValues) ->
	ItemGuids = get_item_guids(SlotValues),
	{Guids, _} = lists:split(?equipment_slot_end, ItemGuids),
	Guids.

% returns guids for all items in inventory
% this includes equipped and in bags
get_item_guids(Guid) when is_number(Guid) ->
	SlotValues = player_state:run_sync_function(Guid, get_item_guids),
	get_item_guids(SlotValues);
get_item_guids(SlotValues) when is_binary(SlotValues) ->
	Guids = extract_slot_values_guids(SlotValues),
	lists:reverse(Guids).

extract_slot_values_guids(SlotValues) ->
	extract_slot_values_guids(SlotValues, []).
extract_slot_values_guids(<<>>, Acc) -> Acc;
extract_slot_values_guids(<<Guid?Q, Rest/binary>>, Acc) ->
	extract_slot_values_guids(Rest, [Guid|Acc]).




equip_new(ItemId, Values, OwnerGuid) ->
	ItemGuid = world:get_guid(?highguid_item, 0),
	ItemValues = item_values:create(ItemGuid, ItemId, OwnerGuid),
	item_data:store_values(ItemValues),
	NewValues = equip_out_of_game(OwnerGuid, ItemId, Values, ItemGuid),
	% TODO get this working for out of game stats update
	%stats:update_all_out_of_game(OwnerGuid, NewValues).
	NewValues.


equip_slot(ItemGuid, DestSlot, OwnerGuid) ->
	player_state:run_async_function(OwnerGuid, set_item, [{DestSlot, ItemGuid}]),

	IsEquipSlot = is_equip_slot(DestSlot),
	if IsEquipSlot ->
			%stats:update_all(OwnerGuid),
			ok;
		true -> ok
	end,

	item_data:set_guids(ItemGuid, OwnerGuid).

equip(ItemGuid, DestSlot, OwnerGuid) ->
	equip_slot(ItemGuid, DestSlot, OwnerGuid),
	update_data:build_create_update_packet_for_items([ItemGuid]).

% need to do everything manually because this is done out of game when a char is created
equip_out_of_game(OwnerGuid, ItemId, Values, NewItemGuid) ->
	ItemProto = content:lookup_item(ItemId),
	Class = ItemProto#item_proto.class,
	if Class == ?item_class_weapon orelse Class == ?item_class_armor ->
			InvType = ItemProto#item_proto.inventory_type,
			Slot = get_slot(InvType),

			item_data:set_guids(NewItemGuid, OwnerGuid),

			ItemId = ItemProto#item_proto.id,
			{NewValues, _} = player_state_functions:set_item(Values, {Slot, NewItemGuid}),
			{OutValues, _} = player_state_functions:set_visible_item(NewValues, {Slot, ItemId}),
			OutValues;
		true ->
			% put item in bag
			item_data:set_guids(NewItemGuid, OwnerGuid),

			Slot = player_state_functions:get_first_empty_inv_slot(Values),
			{NewValues, _} = player_state_functions:set_item(Values, {Slot, NewItemGuid}),
			NewValues
	end.

get_first_empty_inv_slot(OwnerGuid) ->
	player_state:run_sync_function(OwnerGuid, get_first_empty_inv_slot).


visualize_item(OwnerGuid, ItemGuid, Slot) ->
	player_state:run_async_function(OwnerGuid, set_item, [{Slot, ItemGuid}]),

	item_data:set_guids(ItemGuid, OwnerGuid),

	set_visual_item_slot(OwnerGuid, ItemGuid, Slot).


set_visual_item_slot(OwnerGuid, ItemGuid, Slot) ->
	IsEquipSlot = is_equip_slot(Slot),
	if IsEquipSlot ->
			ItemId = if ItemGuid > 0 ->
					ItemValues = item_data:get_values(ItemGuid),
					item_values:get_value(object_field_entry, ItemValues);
				true -> 0
			end,
			player_state:run_async_function(OwnerGuid, set_visible_item, [{Slot, ItemId}]);
		not IsEquipSlot -> ok
	end.



get_slot(InvType) ->
	case InvType of
		?invtype_head -> ?equipment_slot_head;
		?invtype_neck -> ?equipment_slot_neck;
		?invtype_shoulders -> ?equipment_slot_shoulders;
		?invtype_body -> ?equipment_slot_body;
		?invtype_chest -> ?equipment_slot_chest;
		?invtype_waist -> ?equipment_slot_waist;
		?invtype_legs -> ?equipment_slot_legs;
		?invtype_feet -> ?equipment_slot_feet;
		?invtype_wrists -> ?equipment_slot_wrists;
		?invtype_hands -> ?equipment_slot_hands;
		?invtype_finger -> ?equipment_slot_finger1;
		?invtype_trinket -> ?equipment_slot_trinket1;
		?invtype_weapon -> ?equipment_slot_mainhand;
		?invtype_shield -> ?equipment_slot_offhand;
		?invtype_ranged -> ?equipment_slot_ranged;
		?invtype_cloak -> ?equipment_slot_back;
		?invtype_2hweapon -> ?equipment_slot_mainhand;
		?invtype_tabard -> ?equipment_slot_tabard;
		?invtype_robe -> ?equipment_slot_chest;
		?invtype_weaponmainhand -> ?equipment_slot_mainhand;
		?invtype_weaponoffhand -> ?equipment_slot_offhand;
		?invtype_holdable -> ?equipment_slot_mainhand;
		?invtype_thrown -> ?equipment_slot_ranged;
		?invtype_rangedright -> ?equipment_slot_ranged;
		_ -> -1
	end.

is_equippable(ItemProto) ->
	InvType = ItemProto#item_proto.inventory_type,
	Slot = item:get_slot(InvType),
	Slot >= 0.
