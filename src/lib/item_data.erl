-module(item_data).

-export([init/0, cleanup/0]).
-export([store_values/1]).
-export([delete_items/1, delete_item/1]).
-export([get_values/1]).
-export([get_item_proto/1, get_item_id/1]).


init() ->
	dets_store:open(item_values, true),
	ok.

cleanup() ->
	dets_store:close(item_values, true),
	ok.



store_values(ItemValues) ->
	ItemGuid = item_values:get_guid(ItemValues),
	dets_store:store(item_values, {ItemGuid, ItemValues}, true).


get_values(ItemGuid) ->
	case dets_store:lookup(item_values, ItemGuid, true) of
		[] -> throw(badarg);
		[{ItemGuid, Values}] -> Values
	end.


delete_items(GuidList) ->
	lists:foreach(fun(Guid) ->
		delete_item(Guid)
	end, GuidList).

delete_item(Guid) ->
	dets_store:delete(item_values, Guid, true).


%% misc gets
get_item_proto(ItemGuid) ->
	ItemId = get_item_id(ItemGuid),
	content:lookup_item(ItemId).

get_item_id(ItemGuid) ->
	Values = get_values(ItemGuid),
	item_values:get_item_id(Values).
