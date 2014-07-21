-module(player_router_sup).
-behavior(supervisor).

-export([start_link/2]).
-export([init/1]).


start_link(AccountId, SendPid) ->
	supervisor:start_link(?MODULE, {AccountId, SendPid}).

init({AccountId, SendPid}) ->
	Router = player_router,
	Account = player_account,
	{ok, {{one_for_one, 3, 5},
				[
					{Router,
						{Router, start_link, [AccountId, SendPid, self()]},
						transient, 1000, worker, [Router]},
					{Account,
						{Account, start_link, [AccountId]},
						transient, 1000, worker, [Account]}
				]}}.
