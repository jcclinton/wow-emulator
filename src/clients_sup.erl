-module(clients_sup).
-behavior(supervisor).

-export([start_link/0, get_count/0]).
-export([init/1]).
-export([start_child/0]).


get_count() ->
	supervisor:count_children(?MODULE).

start_child() ->
	{ok, Pid} = supervisor:start_child(?MODULE, []),
	Pid.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{simple_one_for_one, 0, 5},
				[{client,
					{client, start_link, []},
					temporary, 10000, worker, [client]}
				]}}.
