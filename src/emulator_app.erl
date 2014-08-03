-module(emulator_app).
-behavior(application).

-export([start/2, stop/1]).


start(normal, _Args) ->
	%application:load(sasl),
	%application:start(sasl),
	%application:start(mnesia),
	%mnesia:wait_for_tables([account, realm, character], 1000),
	application:start(gproc),
	world_data:init(),
	char_data:init(),
	item_data:init(),
	account:init(),
	static_store:init(),
	emulator_sup:start_link().

stop(_State) ->
	application:stop(gproc),
	account:destroy(),
	world_data:cleanup(),
	char_data:cleanup(),
	item_data:cleanup(),
	static_store:cleanup(),
	ok.
