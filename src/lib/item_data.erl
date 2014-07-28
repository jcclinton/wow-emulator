-module(item_data).

-export([init/0, cleanup/0]).
-export([store_values/1]).
-export([get_values/1]).


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
