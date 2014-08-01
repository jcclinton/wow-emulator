-module(player_workers_sup).
-behavior(supervisor).

-export([start_link/1, start_worker/2]).
-export([init/1]).

%% api
start_worker(WorkerData, AccountId) ->
	Pid = get_pid(AccountId),
	supervisor:start_child(Pid, [WorkerData]).


get_pid(AccountId) ->
	world:build_pid(AccountId, "worker_sup").


%%callbacks

start_link(AccountId) ->
	Pid = get_pid(AccountId),
	supervisor:start_link(Pid, ?MODULE, []).


init([]) ->
	Name = player_worker,
	{ok, {{simple_one_for_one, 0, 1},
				[{Name,
					{Name, start_link, []},
					transient, 2000, worker, [Name]}
				]}}.
