-module(player_workers_sup).
-behavior(supervisor).

-export([start_link/1, start_worker/2]).
-export([init/1]).

%% api
start_worker(WorkerData, AccountId) ->
	Pid = util:get_pid(?MODULE, AccountId),
	supervisor:start_child(Pid, [WorkerData]).


%%callbacks

start_link(AccountId) ->
	supervisor:start_link(?MODULE, AccountId).


init(AccountId) ->
	Name = player_worker,
	util:reg_proc(?MODULE, AccountId),
	{ok, {{simple_one_for_one, 0, 1},
				[{Name,
					{Name, start_link, []},
					transient, 2000, worker, [Name]}
				]}}.
