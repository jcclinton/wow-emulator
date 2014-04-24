-module(sockserv_sup).
-behavior(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).


start_link(ListenSocket) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, ListenSocket).

init(ListenSocket) ->
	Procs = getChildSpecs(ListenSocket),,
	{ok, {{one_for_one, 60, 3600},Procs}}.

getChildSpecs(ListenSocket) ->[{sockserv_rcv_sup,
					{sockserv_rcv_sup, start_link, [ListenSocket]},
					permanent, 1000, supervisor, [sockserv_rcv_sup]},
				  {sockserv_send_sup,
					{sockserv_serv, start_link, []},
					permanent, 1000, supervisor, [sockserv_send_sup]},
				  {sockserv_controller,
					{sockserv_controller, start_link, [self()]},
					permanent, 1000, worker, [sockserv_controller]},
					].
