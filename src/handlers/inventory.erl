-module(inventory).

-export([swap_inv_item/1]).
-export([use_item/1]).



-include("include/binary.hrl").


use_item(Data) ->
	_Guid = recv_data:get(guid, Data),
	<<_BagIndex?B, _Slot?B, _SpellCount?B>> = recv_data:get(payload, Data),
	ok.

swap_inv_item(Data) ->
	<<SrcSlot?B, DestSlot?B>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	item:swap(SrcSlot, DestSlot, Guid),
	ok.
