-module(emulator_app).
-behavior(application).

-export([start/2, stop/1]).


start(normal, _Args) ->
	%application:load(sasl),
	%application:start(sasl),
	%application:start(mnesia),
	%mnesia:wait_for_tables([account, realm, character], 1000),
	world_data:init(),
	char_data:init(),
	account:init(),
	object_store:init(),
	emulator_sup:start_link().

stop(_State) ->
	account:destroy(),
	world_data:cleanup(),
	char_data:cleanup(),
	object_store:cleanup(),
	ok.
