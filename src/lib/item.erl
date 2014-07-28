-module(item).

-export([create/2]).
-export([equip_new/3, equip/4]).
-export([init_char_slot_values/0]).
-export([get_item_guids/1, get_equipped_item_guids/1]).

-include("include/types.hrl").
-include("include/binary.hrl").
-include("include/database_records.hrl").
-include("include/character.hrl").
-include("include/items.hrl").




get_equipped_item_guids(Guid) ->
	ItemGuids = get_item_guids(Guid),
	{Guids, _} = lists:split(?equipment_slot_end, ItemGuids),
	Guids.

get_item_guids(Guid) ->
	SlotValues = char_data:get_slot_values(Guid),
	Guids = extract_slot_values_guids(SlotValues),
	lists:reverse(Guids).

extract_slot_values_guids(SlotValues) ->
	extract_slot_values_guids(SlotValues, []).
extract_slot_values_guids(<<>>, Acc) -> Acc;
extract_slot_values_guids(<<Guid?Q, Rest/binary>>, Acc) ->
	extract_slot_values_guids(Rest, [Guid|Acc]).




init_char_slot_values() ->
	binary:copy(<<0?Q>>, ?player_slots_count).



equip_new(ItemId, CharSlotValues, OwnerGuid) ->
	ItemValues = create(ItemId, OwnerGuid),
	ItemGuid = item_values:get_guid(ItemValues),
	NewSlotValues = equip(ItemId, CharSlotValues, ItemGuid, false),
	{NewSlotValues, ItemValues}.


equip(ItemId, SlotValues, NewItemGuid, Swap) ->
	ItemProto = content:lookup_item(ItemId),
	Class = ItemProto#item_proto.class,
	if Class == ?item_class_weapon orelse Class == ?item_class_armor ->
		InvType = ItemProto#item_proto.inventory_type,
		Slot = get_slot(InvType),
		if Slot >= 0 ->
				% offset is in 64 bit chunks
				Offset = Slot * 8,
				<<Head:Offset/binary, OldItemGuid?Q, Rest/binary>> = SlotValues,
				if OldItemGuid == 0 orelse Swap ->
						<<Head/binary, NewItemGuid?Q, Rest/binary>>;
					true ->
						SlotValues
				end;
			true -> SlotValues
			end;
		true -> SlotValues
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


create(ItemId, OwnerGuid) ->
	ItemGuid = world:get_guid(?highguid_item, 0),

	init_values(ItemGuid, ItemId, OwnerGuid).



init_values(ItemGuid, ItemId, OwnerGuid) ->
	ObjectType = ?typemask_item bor ?typemask_object,
	Scale = 1,

	%ItemProto = content:get_item(ItemId),
	%ItemMaxDurability = ItemProto#item.max_durability,
	ItemMaxDurability = 10,

	KeyValues = [
		{'OBJECT_FIELD_GUID', ItemGuid, uint64},
		{'OBJECT_FIELD_TYPE', ObjectType, uint32},
    {'OBJECT_FIELD_SCALE_X', Scale, float},
    {'OBJECT_FIELD_ENTRY', ItemId, uint32},

    {'ITEM_FIELD_OWNER', OwnerGuid, uint64},
    {'ITEM_FIELD_CONTAINED', OwnerGuid, uint64},
    {'ITEM_FIELD_STACK_COUNT', 1, uint32},
    {'ITEM_FIELD_MAXDURABILITY', ItemMaxDurability, uint32},
    {'ITEM_FIELD_DURABILITY', ItemMaxDurability, uint32}
	],

	EmptyValues = get_empty_values(),
	lists:foldl(fun object_values:set_key_values/2, EmptyValues, KeyValues).


get_empty_values() ->
	TotalCount = update_fields:get_total_count(item),
	binary:copy(<<0?L>>, TotalCount).
