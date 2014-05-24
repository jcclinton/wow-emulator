-module(emulator_app).
-behavior(application).

-export([start/2, stop/1]).


start(normal, _Args) ->
	application:start(mnesia),
	%mnesia:wait_for_tables([account, realm, character], 1000),
	ets:new(connected_clients, [named_table, set, public]),
	account:init(),
	emulator_sup:start_link().

stop(_State) ->
	account:destroy(),
	ets:delete(connected_clients),
	ok.
