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

-module(clients_sup).
-behavior(supervisor).

-export([start_link/0, get_count/0]).
-export([init/1]).
-export([start_child/0, start_child/1]).


get_count() ->
	supervisor:count_children(?MODULE).

start_child() ->
	AccountId = client_controller:get_dummy_account(),
	start_child(AccountId).
start_child(AccountId) ->
	{ok, Pid} = supervisor:start_child(?MODULE, [AccountId]),
	Pid.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{simple_one_for_one, 0, 5},
				[{client_sup,
					{client_sup, start_link, []},
					temporary, 1000, worker, [client_sup]}
				]}}.
