-module(static_store).

-export([init/0, cleanup/0]).
-export([store_new/2]).
-export([
	item_class_lookup/1,
	spell_lookup/1,
	start_outfit_lookup/3
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

spell_lookup(Id) ->
	Tab = spell_store,
	lookup(Tab, Id).

item_class_lookup(Id) ->
	Tab = item_class_store,
	lookup(Tab, Id).

start_outfit_lookup(Race, Class, Gender) ->
	All = ets:match_object(char_start_outfit_store, {'_', '_'}),
	start_outfit_lookup(Race, Class, Gender, All).

start_outfit_lookup(_, _, _, []) -> throw(badarg);
start_outfit_lookup(Race, Class, Gender, [{_, Record}|Rest]) ->
		RaceVal =  Record#char_start_outfit_store.race,
		ClassVal =  Record#char_start_outfit_store.class,
		GenderVal =  Record#char_start_outfit_store.gender,
		if RaceVal == Race andalso ClassVal == Class andalso GenderVal == Gender ->
				Record#char_start_outfit_store.item_ids;
			true ->
				start_outfit_lookup(Race, Class, Gender, Rest)
		end.



% private
lookup(Tab, Id) ->
	case ets:lookup(Tab, Id) of
		[{Id, Record}] -> Record;
		[] -> nil
	end.
