-module(worldserv_sup).
-behavior(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, Port} = application:get_env(world_port),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, binary]),
	spawn_link(fun empty_listeners/0),
	{ok, {{simple_one_for_one, 60, 3600},
				[{socket,
					{worldserv_serv, start_link, [ListenSocket]},
					transient, 1000, worker, [worldserv_serv]}
				]}}.


start_socket() ->
	supervisor:start_child(?MODULE, []).

empty_listeners() ->
	[start_socket() || _ <- lists:seq(1,1)],
	ok.
