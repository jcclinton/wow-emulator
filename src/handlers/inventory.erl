-module(inventory).

-export([swap_inv_item/1]).
-export([use_item/1]).
-export([item_query_single/1]).
-export([autoequip_item/1]).
-export([split_item/1]).


-include("include/binary.hrl").
-include("include/items.hrl").
-include("include/database_records.hrl").


split_item(Data) ->
	Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<_SrcBag?B, SrcSlot?B, _DestBag?B, DestSlot?B, Count?B>> = Payload,
	io:format("split payload ~p~n", [Payload]),

	CanSplit = item:can_split(SrcSlot, DestSlot, Count, Guid),

	if CanSplit ->
			item:split(SrcSlot, DestSlot, Count, Guid);
		true ->
			Error = ?equip_err_couldnt_split_items,
			return_error(SrcSlot, DestSlot, Guid, Error)
	end.




autoequip_item(Data) ->
	Guid = recv_data:get(guid, Data),
	<<_SrcBag?B, SrcSlot?B>> = recv_data:get(payload, Data),
	ItemGuid = item:get_item_guid_at_slot(SrcSlot, Guid),
	ItemProto = item_data:get_item_proto(ItemGuid),
	InvType = ItemProto#item_proto.inventory_type,
	DestSlot = case item:get_slot(InvType) of
		-1 -> item:get_first_empty_inv_slot(Guid);
		Num -> Num
	end,
	swap_internal(SrcSlot, DestSlot, Guid).

use_item(Data) ->
	_Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<_BagIndex?B, _Slot?B, _SpellCount?B, Rest/binary>> = Payload,
	io:format("use item: ~p~n", [Payload]),
	ok.

item_query_single(Data) ->
	<<ItemId?L, ItemGuid?Q>> = recv_data:get(payload, Data),
	io:format("query item: ~p~nitem guid: ~p~n", [ItemId, ItemGuid]),
	ok.

swap_inv_item(Data) ->
	<<SrcSlot?B, DestSlot?B>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	swap_internal(SrcSlot, DestSlot, Guid).





%% private

swap_internal(SrcSlot, DestSlot, Guid) ->
	if SrcSlot < 0 ->
			Error = ?equip_err_bag_full,
			return_error(SrcSlot, DestSlot, Guid, Error);
		SrcSlot >= 0 ->
			case item:swap(SrcSlot, DestSlot, Guid) of
				{error, Error} ->
					return_error(SrcSlot, DestSlot, Guid, Error);
				ok -> ok;
				Msg -> Msg
			end
	end.


return_error(SrcSlot, DestSlot, Guid, Error) ->
	Level = if Error == ?equip_err_cant_equip_level_i -> <<100?L>>;
		true -> <<>>
	end,

	SrcEmpty = item:slot_empty(SrcSlot, Guid),
	DestEmpty = item:slot_empty(DestSlot, Guid),
	SrcItemGuid = if not SrcEmpty ->
			item:get_item_guid_at_slot(SrcSlot, Guid);
		SrcEmpty ->
			Guid
	end,
	DestItemGuid = if not DestEmpty ->
			item:get_item_guid_at_slot(DestSlot, Guid);
		DestEmpty ->
			Guid
	end,

	Payload = <<Error?B, Level/binary, SrcItemGuid?Q, DestItemGuid?Q, 0?B>>,
	{smsg_inventory_change_failure, Payload}.
