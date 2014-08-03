-module(emulator_app).
-behavior(application).

-export([start/2, stop/1]).


start(normal, _Args) ->
	char_data:init(),
	item_data:init(),
	world_data:init(),
	account:init(),
	static_store:init(),

	emulator_sup:start_link().

stop(_State) ->
	char_data:cleanup(),
	item_data:cleanup(),
	world_data:cleanup(),
	account:destroy(),
	static_store:cleanup(),
	ok.
