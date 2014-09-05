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

-module(inventory).

-export([swap_inv_item/1]).
-export([use_item/1]).
-export([item_query_single/1]).
-export([autoequip_item/1]).
-export([split_item/1]).
-export([destroy_item/1]).


-include("include/binary.hrl").
-include("include/items.hrl").
-include("include/database_records.hrl").
-include("include/data_types.hrl").


% destroy an item at a slot
-spec destroy_item(recv_data()) -> handler_response().
destroy_item(Data) ->
	Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<_SrcBag?B, SrcSlot?B, Count?B, _Data1?B, _Data2?B, _Data3?B>> = Payload,
	case item:destroy(SrcSlot, Count, Guid) of
		{error, Error} ->
			return_error(SrcSlot, 0, Guid, Error);
		ok -> ok;
		Msg -> Msg
	end.

% split an item from one slot to another
-spec split_item(recv_data()) -> handler_response().
split_item(Data) ->
	Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<_SrcBag?B, SrcSlot?B, _DestBag?B, DestSlot?B, Count?B>> = Payload,

	CanSplit = item:can_split(SrcSlot, DestSlot, Count, Guid),

	if CanSplit ->
			item:split(SrcSlot, DestSlot, Count, Guid);
		true ->
			Error = ?equip_err_couldnt_split_items,
			return_error(SrcSlot, DestSlot, Guid, Error)
	end.



% autoequip an item from a slot
-spec autoequip_item(recv_data()) -> handler_response().
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

% use item at a slot
-spec use_item(recv_data()) -> handler_response().
use_item(Data) ->
	_Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<_BagIndex?B, _Slot?B, _SpellCount?B, _Rest/binary>> = Payload,
	io:format("use item: ~p~n", [Payload]),
	ok.

% return full item prototype for an unknown item
-spec item_query_single(recv_data()) -> handler_response().
item_query_single(Data) ->
	<<ItemId?L, ItemGuid?Q>> = recv_data:get(payload, Data),
	io:format("unknown item: ~p with item guid: ~p~n", [ItemId, ItemGuid]),
	ok.

% swap an item from one slot to another
-spec swap_inv_item(recv_data()) -> handler_response().
swap_inv_item(Data) ->
	<<SrcSlot?B, DestSlot?B>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	swap_internal(SrcSlot, DestSlot, Guid).





%% private

-spec swap_internal(non_neg_integer(), non_neg_integer(), guid()) -> handler_response().
swap_internal(SrcSlot, DestSlot, Guid) ->
	case item:swap(SrcSlot, DestSlot, Guid) of
		{error, Error} ->
			return_error(SrcSlot, DestSlot, Guid, Error);
		ok -> ok;
		Msg -> Msg
	end.


-spec return_error(non_neg_integer(), non_neg_integer(), guid(), term()) -> handler_response().
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
