-module(client_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(Account) ->
	supervisor:start_link(?MODULE, {Account}).

init({Account}) ->
	Procs = getChildSpecs(Account),
	% this supervisor wont restart its children if one dies
	% the reason is that if a process containing the socket dies,
	% then the socket will get closed, then the client will disconnect
	% and then the client will have to go through authentication anyway
	% so there is no point in keeping these processes around
	{ok, {{one_for_all, 0, 1}, Procs}}.

getChildSpecs(Account) ->[
				{client_rcv,
					{client_rcv, start_link, [self(), Account]},
					transient, 10000, worker, [client_rcv]}
				].
