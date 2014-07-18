-module(emulator_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	UseDummyClient = false,
	ClientProc = {auth_client,
								{auth_client, start_link, []},
									permanent, 10000, worker, [auth_client]},
	% eventually login server and world server will have to be split off from each other
	% for now its ok to all be under the emulator_sup
	Procs = [{login_server_sup,
					{login_server_sup, start_link, []},
					permanent, 10000, supervisor, [login_server_sup]},
				 {players_sup,
					{players_sup, start_link, []},
					permanent, 10000, supervisor, [players_sup]},
				 {clients_sup,
					{clients_sup, start_link, []},
					permanent, 10000, supervisor, [clients_sup]},
				 {world,
					{world, start_link, []},
					permanent, 10000, worker, [world]}],
	FinalProcs = if UseDummyClient -> Procs ++ [ClientProc]; true -> Procs end,
	{ok, {{one_for_one, 60, 3600}, FinalProcs}}.
