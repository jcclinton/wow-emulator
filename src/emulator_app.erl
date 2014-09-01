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

-module(emulator_app).
-behavior(application).

-export([start/2, stop/1]).


start(normal, _Args) ->
	% eventually put this somewhere else, but for now this is fine
	Lic1 = "World of Warcraft emulator, Copyright (C) 2014 Jamie Clinton.",
	Lic2 = "This program comes with ABSOLUTELY NO WARRANTY.",
	io:format("~n~p~n~p~n~n", [Lic1, Lic2]),
	char_data:init(),
	char_sess:init(),
	item_data:init(),
	world_data:init(),
	account:init(),
	static_store:init(),

	emulator_sup:start_link().

stop(_State) ->
	char_data:cleanup(),
	char_sess:cleanup(),
	item_data:cleanup(),
	world_data:cleanup(),
	account:destroy(),
	static_store:cleanup(),
	ok.
