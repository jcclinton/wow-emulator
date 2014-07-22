-module(world_data).

-export([init/0, cleanup/0]).
-export([increment_guid/0]).



init() ->
	ets:new(world, [named_table, set, public]),
	ok.

cleanup() ->
	ets:delete(world),
	ok.

increment_guid() ->
	Guid = case ets:lookup(world, guid) of
		[] -> 1;
		[{guid, Num}] -> Num
	end,
	ets:insert(world, {guid, Guid + 1}),
	Guid.
