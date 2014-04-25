-module(sockserv_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	{ok, Port} = application:get_env(realm_port),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, binary]),
	Pid = self(),
	spawn_link(fun() ->
		empty_listeners(Pid)
	end),
	{ok, {{simple_one_for_one, 60, 3600},
				[{sockserv_serv,
					{sockserv_serv, start_link, [ListenSocket]},
					transient, 1000, worker, [sockserv_serv]}
				]}}.


start_socket(Pid) ->
	supervisor:start_child(Pid, []).

empty_listeners(Pid) ->
	[start_socket(Pid) || _ <- lists:seq(1,1)],
	ok.
