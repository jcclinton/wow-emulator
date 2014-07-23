-module(world_data).

-export([init/0, cleanup/0]).
-export([increment_guid/0]).



init() ->
	Tab = world,
	ets:new(Tab, [named_table, set, public]),

	dets:open_file(Tab, [{file, "./db/world.dets"}]),
	dets:to_ets(Tab, Tab),
	ok.

cleanup() ->
	Tab = world,

	ets:delete(Tab),
	dets:close(Tab),
	ok.

increment_guid() ->
	Tab = world,
	Guid = case ets:lookup(Tab, guid) of
		[] -> 1;
		[{guid, Num}] -> Num
	end,
	StoreGuid = Guid + 1,
	ets:insert(Tab, {guid, StoreGuid}),
	dets:insert(Tab, {guid, StoreGuid}),
	Guid.
