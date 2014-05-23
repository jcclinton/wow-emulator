-module(sockserv_send_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	{ok, {{simple_one_for_one, 3, 5},
		[{sockserv_send,
		  {sockserv_send, start_link, []},
		  transient, 1000, worker, [sockserv_send]}
		]}}.
