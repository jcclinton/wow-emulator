-module(clients_sup).
-behavior(supervisor).

-export([start_link/0, get_count/0]).
-export([init/1]).
-export([start_child/0]).


get_count() ->
	supervisor:count_children(?MODULE).

start_child() ->
	supervisor:start_child(?MODULE, []).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{simple_one_for_one, 2, 5},
				[{client,
					{client, start_link, []},
					transient, 10000, worker, [client]}
				]}}.
