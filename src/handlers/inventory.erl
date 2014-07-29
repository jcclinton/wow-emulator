-module(inventory).

-export([swap_inv_item/1]).



swap_inv_item(Data) ->
	<<SrcSlot?B, DestSlot?B>> = recv_data:get(payload, Data),
	ok.
