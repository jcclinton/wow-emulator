-module(player_controller_sup).
-behavior(supervisor).

-export([start_link/2]).
-export([init/1]).


start_link(AccountId, SendPid) ->
	supervisor:start_link(?MODULE, {AccountId, SendPid}).

init({AccountId, SendPid}) ->
	{ok, {{one_for_one, 2, 5},
				[{player_controller,
					{player_controller, start_link, [AccountId, SendPid]},
					transient, 10000, worker, [player_controller]}
				]}}.
