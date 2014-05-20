-module(client_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	UseDummyClient = false,
	ClientProc = {client,
								{client, start_link, []},
									transient, 10000, worker, [client]},
	Procs = [{sockserv_sup,
					{sockserv_sup, start_link, []},
					transient, 10000, supervisor, [sockserv_sup]},
				 {worldserv_sup,
					{worldserv_sup, start_link, []},
					transient, 10000, supervisor, [worldserv_sup]}],
	FinalProcs = if UseDummyClient -> Procs ++ [ClientProc]; true -> Procs end,
	{ok, {{one_for_one, 60, 3600}, FinalProcs}}.
