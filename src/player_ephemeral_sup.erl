-module(player_ephemeral_sup).
-behavior(supervisor).

-export([start_link/2]).
-export([init/1]).


start_link(AccountId, Guid) ->
	supervisor:start_link(?MODULE, {AccountId, Guid}).

init({AccountId, Guid}) ->
	{ok, {{one_for_one, 5, 8},
				[
					{player_updater,
						{player_updater, start_link, [AccountId, Guid]},
						transient, 1000, worker, [player_updater]},

					{player_spell,
						{player_spell, start_link, [AccountId, Guid]},
						transient, 1000, worker, [player_spell]},

					{player_melee,
						{player_melee, start_link, [AccountId, Guid]},
						transient, 1000, worker, [player_melee]}
				]}}.
