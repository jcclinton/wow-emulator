-module(sockserv_app).
-behavior(application).

-export([start/2, stop/1]).


start(normal, _Args) ->
	client_sup:start_link().

stop(_State) ->
	ok.
