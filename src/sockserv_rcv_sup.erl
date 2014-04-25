-module(sockserv_rcv_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(ListenSocket) ->
	supervisor:start_link(?MODULE, ListenSocket).

init(ListenSocket) ->
	{ok, {{simple_one_for_one, 60, 3600},
		[{sockserv_rcv,
		  {sockserv_rcv, start_link, [ListenSocket]},
		  permanent, 1000, worker, [sockserv_rcv]}
		]}}.