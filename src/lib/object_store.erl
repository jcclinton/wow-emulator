-module(object_store).

-export([init/0, cleanup/0]).
-export([store_new/2]).
-export([area_lookup/1]).


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
	ets:store_new(Tab, Data).


% public api

area_lookup(Id) ->
	ets:lookup(area_store, Id).



% private
get_tabs() ->
	[
		area_store,
		area_trigger_store,
		auction_house_store,
		bank_bag_slot_prices_store
	].
