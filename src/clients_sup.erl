-module(clients_sup).
-behavior(supervisor).

-export([start_link/0, get_count/0]).
-export([init/1]).
-export([start_child/0, start_child/1]).


get_count() ->
	supervisor:count_children(?MODULE).

start_child() ->
	AccountId = client_controller:get_dummy_account(),
	start_child(AccountId).
start_child(AccountId) ->
	{ok, Pid} = supervisor:start_child(?MODULE, [AccountId]),
	Pid.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{simple_one_for_one, 0, 5},
				[{client_sup,
					{client_sup, start_link, []},
					temporary, 1000, worker, [client_sup]}
				]}}.
