-module(sockserv_send_sup).
-behavior(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).


start_link([]) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{simple_one_for_one, 60, 3600},
		[{sockserv_send,
		  {sockserv_send, start_link, [ListenSocket]},
		  permanent, 1000, worker, [sockserv_send]},
		]}}.
