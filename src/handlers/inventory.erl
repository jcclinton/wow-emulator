-module(inventory).

-export([swap_inv_item/1]).

-include("include/binary.hrl").


swap_inv_item(Data) ->
	<<SrcSlot?B, DestSlot?B>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	item:swap(SrcSlot, DestSlot, Guid),
	ok.
