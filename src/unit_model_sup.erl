-module(unit_model_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(Guid) ->
	supervisor:start_link(?MODULE, {Guid}).

init({Guid}) ->
	{ok, {{one_for_one, 3, 5},
				[
					{player_state,
						{player_state, start_link, [Guid]},
						transient, 1000, worker, [player_state]}
				]}}.
