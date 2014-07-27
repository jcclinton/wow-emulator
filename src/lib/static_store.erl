-module(static_store).

-export([init/0, cleanup/0]).
-export([store_new/2]).
-export([
	lookup_item_class/1,
	lookup_spell/1,
	lookup_start_outfit/3
]).

-include("include/database_records.hrl").

get_tabs() ->
	[
		item_class_store,
		spell_store,
		char_start_outfit_store
	].



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

lookup_spell(Id) ->
	Tab = spell_store,
	lookup(Tab, Id).

lookup_item_class(Id) ->
	Tab = item_class_store,
	lookup(Tab, Id).

lookup_start_outfit(Race, Class, Gender) ->
	case lookup(char_start_outfit_store, {Race, Class, Gender}) of
		nil -> [];
		List -> List
	end.



% private
lookup(Tab, Id) ->
	case ets:lookup(Tab, Id) of
		[{Id, Record}] -> Record;
		[] -> nil
	end.
