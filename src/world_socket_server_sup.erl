-module(world_socket_server_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(ListenSocket) ->
	supervisor:start_link(?MODULE, ListenSocket).

init(ListenSocket) ->
	Procs = getChildSpecs(ListenSocket),
	{ok, {{one_for_one, 3, 5},Procs}}.

getChildSpecs(ListenSocket) ->[
				{world_socket_rcv_sup,
					{world_socket_rcv_sup, start_link, [ListenSocket]},
					transient, 1000, supervisor, [world_socket_rcv_sup]},
				{world_socket_send_sup,
					{world_socket_send_sup, start_link, []},
					transient, 1000, supervisor, [world_socket_send_sup]},
				{world_player,
					{world_player, start_link, []},
					transient, 1000, worker, [world_player]},
				{world_socket_controller,
					{world_socket_controller, start_link, [self()]},
					transient, 1000, worker, [world_socket_controller]}
					].
