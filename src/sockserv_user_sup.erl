-module(sockserv_user_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(ListenSocket) ->
	supervisor:start_link(?MODULE, ListenSocket).

init(ListenSocket) ->
	BPub = logon_lib:getServerPublic(),
	SKey = logon_lib:computeServerKey(BPub),
	Procs = getChildSpecs(ListenSocket, SKey),
	{ok, {{one_for_one, 3, 5},Procs}}.

getChildSpecs(ListenSocket, SKey) ->[{sockserv_rcv_sup,
					{sockserv_rcv_sup, start_link, [ListenSocket]},
					permanent, 1000, supervisor, [sockserv_rcv_sup]},
				  {sockserv_send_sup,
					{sockserv_send_sup, start_link, []},
					permanent, 1000, supervisor, [sockserv_send_sup]},
				  {sockserv_controller,
					{sockserv_controller, start_link, [self(), SKey]},
					permanent, 1000, worker, [sockserv_controller]}
					].
