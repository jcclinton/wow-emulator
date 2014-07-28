-module(item).

-export([create/2]).
-export([equip_new/3, equip/4]).
-export([init_char_slot_values/0]).

-include("include/types.hrl").
-include("include/binary.hrl").
-include("include/database_records.hrl").
-include("include/character.hrl").



init_char_slot_values() ->
	binary:copy(<<0?Q>>, ?player_slots_count).



equip_new(ItemId, CharSlotValues, OwnerGuid) ->
	ItemValues = create(ItemId, OwnerGuid),
	ItemGuid = item_values:get_guid(ItemValues),
	ItemProto = content:lookup_item(ItemId),
	InvType = ItemProto#item_proto.inventory_type,
	NewSlotValues = equip(CharSlotValues, ItemGuid, InvType, false),
	{NewSlotValues, ItemValues}.


equip(SlotValues, NewItemGuid, InventoryType, Swap) ->
	% offset is in 64 bit chunks
	Offset = InventoryType * 8,
	<<Head:Offset/binary, OldItemGuid?Q, Rest/binary>> = SlotValues,
	if OldItemGuid == 0 orelse Swap ->
			<<Head/binary, NewItemGuid?Q, Rest/binary>>;
		true ->
			SlotValues
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
