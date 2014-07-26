-module(object_store).

-export([init/0, cleanup/0]).
-export([store_new/2]).
-export([item_class_lookup/1]).

-include("include/database_records.hrl").

init() ->
	Tabs = get_tabs(),
	lists:foreach(fun(Tab) ->
		ets:new(Tab, [named_table, set, public])
	end, Tabs),

	dbc_loader:load_all(),
	ok.


cleanup() ->
	Tabs = get_tabs(),
	lists:foreach(fun(Tab) ->
		ets:delete(Tab)
	end, Tabs).


store_new(Tab, Data) ->
	ets:insert_new(Tab, Data).


% public api

item_class_lookup(Id) ->
	case ets:lookup(item_class_store, Id) of
		[{Id, Record}] -> Record;
		[] -> nil
	end.



% private
get_tabs() ->
	[
		item_class_store
	].
