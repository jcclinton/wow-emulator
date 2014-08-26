-module(player_model_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(AccountId) ->
	supervisor:start_link(?MODULE, {AccountId}).

init({AccountId}) ->
	{ok, {{one_for_one, 3, 5},
				[
					{player_model_controller,
						{player_model_controller, start_link, [AccountId, self()]},
						transient, 1000, worker, [player_model_controller]}
				]}}.
