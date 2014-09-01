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

-module(emulator_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	%UseDummyClient = false,
	%ClientProc = {auth_client,
								%{auth_client, start_link, []},
									%permanent, 10000, worker, [auth_client]},
	% eventually login server and world server will have to be split off from each other
	% for now its ok to all be under the emulator_sup
	Procs = [{login_server_sup,
					{login_server_sup, start_link, []},
					permanent, 10000, supervisor, [login_server_sup]},
				 {players_sup,
					{players_sup, start_link, []},
					permanent, 10000, supervisor, [players_sup]},
				 {clients_sup,
					{clients_sup, start_link, []},
					permanent, 10000, supervisor, [clients_sup]},
				 {world,
					{world, start_link, []},
					permanent, 10000, worker, [world]}],
	%FinalProcs = if UseDummyClient -> Procs ++ [ClientProc]; true -> Procs end,
	{ok, {{one_for_one, 60, 3600}, Procs}}.
