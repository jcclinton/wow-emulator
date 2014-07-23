-module(dets_store).

-export([open/1, close/1, store/2, store_new/2, delete/2, lookup/2]).
-export([open/2, close/2, store/3, store_new/3, delete/3, lookup/3]).

open(Tab) -> open(Tab, false).
open(Tab, UseEts) ->
	File = get_dets_file(Tab),
	dets:open_file(Tab, [{file, File}]),
	if UseEts ->
			ets:new(Tab, [named_table, set, public]),
			dets:to_ets(Tab, Tab);
		true -> ok
	end.

close(Tab) -> close(Tab, false).
close(Tab, HasEts) ->
	if HasEts ->
			ets:delete(Tab);
		true -> ok
	end,
	dets:close(Tab).


lookup(Tab, Key) -> lookup(Tab, Key, false).
lookup(Tab, Key, HasEts) ->
	if HasEts ->
			ets:lookup(Tab, Key);
		true ->
			dets:lookup(Tab, Key)
	end.


delete(Tab, Key) -> delete(Tab, Key, false).
delete(Tab, Key, HasEts) ->
	if HasEts ->
			ets:delete(Tab, Key);
		true -> ok
	end,
	dets:delete(Tab, Key).



store_new(Tab, Data) -> store(Tab, Data, false).
store_new(Tab, Data, HasEts) ->
	if HasEts ->
			ets:insert_new(Tab, Data);
		true -> ok
	end,
	dets:insert_new(Tab, Data).

store(Tab, Data) -> store(Tab, Data, false).
store(Tab, Data, HasEts) ->
	if HasEts ->
			ets:insert(Tab, Data);
		true -> ok
	end,
	io:format("data: ~p~n", [Data]),
	dets:insert(Tab, Data).



get_dets_file(Tab) ->
	"./db/"++ atom_to_list(Tab) ++ ".dets".
