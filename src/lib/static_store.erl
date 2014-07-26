-module(static_store).

-export([init/0, cleanup/0]).
-export([store_new/2]).
-export([
	item_class_lookup/1,
	spell_lookup/1
]).

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

spell_lookup(Id) ->
	Tab = spell_store,
	lookup(Tab, Id).

item_class_lookup(Id) ->
	Tab = item_class_store,
	lookup(Tab, Id).



% private
get_tabs() ->
	[
		item_class_store,
		spell_store
	].

lookup(Tab, Id) ->
	case ets:lookup(Tab, Id) of
		[{Id, Record}] -> Record;
		[] -> nil
	end.
