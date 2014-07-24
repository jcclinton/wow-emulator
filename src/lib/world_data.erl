-module(world_data).

-export([init/0, cleanup/0]).
-export([increment_guid/0]).



init() ->
	dets_store:open(world, true),
	ok.

cleanup() ->
	dets_store:close(world, true),
	ok.

increment_guid() ->
	Tab = world,
	Guid = case dets_store:lookup(Tab, guid, true) of
		[] -> 1;
		[{guid, Num}] -> Num
	end,
	StoreGuid = Guid + 1,
	dets_store:store(Tab, {guid, StoreGuid}, true),
	Guid.
