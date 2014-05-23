-module(emulator_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	UseDummyClient = false,
	ClientProc = {client,
								{client, start_link, []},
									permanent, 10000, worker, [client]},
	Procs = [{login_server_sup,
					{login_server_sup, start_link, []},
					permanent, 10000, supervisor, [login_server_sup]},
				 {world_server_sup,
					{world_server_sup, start_link, []},
					permanent, 10000, supervisor, [world_server_sup]}],
	FinalProcs = if UseDummyClient -> Procs ++ [ClientProc]; true -> Procs end,
	{ok, {{one_for_one, 60, 3600}, FinalProcs}}.
