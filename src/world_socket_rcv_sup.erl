-module(world_socket_rcv_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(ListenSocket) ->
	supervisor:start_link(?MODULE, ListenSocket).

init(ListenSocket) ->
	{ok, {{simple_one_for_one, 3, 5},
		[{world_socket_rcv,
		  {world_socket_rcv, start_link, [ListenSocket]},
		  transient, 1000, worker, [world_socket_rcv]}
		]}}.
