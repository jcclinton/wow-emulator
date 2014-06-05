-module(world_socket_server_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(ListenSocket) ->
	supervisor:start_link(?MODULE, ListenSocket).

init(ListenSocket) ->
	Procs = getChildSpecs(ListenSocket),
	{ok, {{one_for_all, 0, 5}, Procs}}.

getChildSpecs(ListenSocket) ->[
				{world_socket_controller,
					{world_socket_controller, start_link, [self(), ListenSocket]},
					transient, 10000, worker, [world_socket_controller]}
					].
