-module(unit_ephemeral_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(Guid) ->
	supervisor:start_link(?MODULE, {Guid}).

init({Guid}) ->
	{ok, {{one_for_one, 5, 8},
				[
					{unit_spell,
						{unit_spell, start_link, [Guid]},
						transient, 1000, worker, [unit_spell]},

					{unit_melee,
						{unit_melee, start_link, [Guid]},
						transient, 1000, worker, [unit_melee]}

					%{player_state,
						%{player_state, start_link, [Guid]},
						%transient, 1000, worker, [player_state]}
				]}}.
