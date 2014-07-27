-module(item).

-export([create/2]).

-include("include/types.hrl").
-include("include/binary.hrl").


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

	TotalCount = update_fields:get_total_count(item),
	% create initially empty binary values object
	EmptyValues = binary:copy(<<0?L>>, TotalCount),
	lists:foldl(fun object_values:set_key_values/2, EmptyValues, KeyValues).
