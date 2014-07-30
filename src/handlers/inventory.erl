-module(inventory).

-export([swap_inv_item/1]).
-export([use_item/1]).
-export([item_query_single/1]).


-include("include/binary.hrl").
-include("include/items.hrl").


use_item(Data) ->
	_Guid = recv_data:get(guid, Data),
	<<_BagIndex?B, _Slot?B, _SpellCount?B>> = recv_data:get(payload, Data),
	ok.

item_query_single(Data) ->
	<<Item?L, Rest/binary>> = recv_data:get(payload, Data),
	io:format("query item: ~p~nrest: ~p~n", [Item, Rest]),
	ok.

swap_inv_item(Data) ->
	<<SrcSlot?B, DestSlot?B>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	case item:swap(SrcSlot, DestSlot, Guid) of
		{error, Error} ->
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

			io:format("sending non empty failure~n"),
			Payload = <<Error?B, Level/binary, SrcItemGuid?Q, DestItemGuid?Q, 0?B>>,
			{smsg_inventory_change_failure, Payload};
		ok -> ok;
		Msg -> Msg
	end.
