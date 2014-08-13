-module(player_controller_sup).
-behavior(supervisor).

-export([start_link/2]).
-export([init/1]).


start_link(AccountId, SendPid) ->
	supervisor:start_link(?MODULE, {AccountId, SendPid}).

init({AccountId, SendPid}) ->
	Controller = player_controller,
	{ok, {{one_for_one, 3, 5},
				[
					{Controller,
						{Controller, start_link, [AccountId, SendPid, self()]},
						transient, 1000, worker, [Controller]}
				]}}.
