%%   This is a World of Warcraft emulator written in erlang, supporting
%%   client 1.12.x
%%
%%   Copyright (C) 2014  Jamie Clinton <jamieclinton.com>
%%
%%   This program is free software; you can redistribute it and/or modify
%%   it under the terms of the GNU General Public License as published by
%%   the Free Software Foundation; either version 2 of the License, or
%%   (at your option) any later version.
%%
%%   This program is distributed in the hope that it will be useful,
%%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%   GNU General Public License for more details.
%%
%%   You should have received a copy of the GNU General Public License along
%%   with this program; if not, write to the Free Software Foundation, Inc.,
%%   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%
%%   World of Warcraft, and all World of Warcraft or Warcraft art, images,
%%   and lore ande copyrighted by Blizzard Entertainment, Inc.

-module(unit_model_sup).
-behavior(supervisor).

-export([start_link/2]).
-export([init/1]).


start_link(Guid, Type) ->
	supervisor:start_link(?MODULE, {Guid, Type}).

init({Guid, Type}) ->
	{ok, {{one_for_one, 3, 5},
				[
					{unit_updater,
						{unit_updater, start_link, [Guid, Type]},
						transient, 1000, worker, [unit_updater]},

					{player_state,
						{player_state, start_link, [Guid]},
						transient, 1000, worker, [player_state]}
				]}}.
