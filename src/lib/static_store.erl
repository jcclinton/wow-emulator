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

-module(static_store).

-export([init/0, cleanup/0]).
-export([store_new/2]).
-export([
	lookup_item_class/1,
	lookup_spell/1,
	lookup_start_outfit/3, lookup_start_outfit/4
]).

-include("include/database_records.hrl").

get_tabs() ->
	[
		item_class_store,
		spell_store,
		char_start_outfit_store
	].



init() ->
	Tabs = get_tabs(),
	lists:foreach(fun(Tab) ->
		ets:new(Tab, [named_table, set, public])
	end, Tabs),

	dbc_loader:load_all(),
	ok.


cleanup() ->
	Tabs = get_tabs(),
	lists:foreach(fun(Tab) ->
		ets:delete(Tab)
	end, Tabs).


store_new(Tab, Data) ->
	ets:insert_new(Tab, Data).



% public api

lookup_spell(Id) ->
	Tab = spell_store,
	lookup(Tab, Id).

lookup_item_class(Id) ->
	Tab = item_class_store,
	lookup(Tab, Id).

lookup_start_outfit(Race, Class, Gender) ->
	lookup_start_outfit(Race, Class, Gender, false).
lookup_start_outfit(Race, Class, Gender, Filter) ->
	Ids = case lookup(char_start_outfit_store, {Race, Class, Gender}) of
		none -> [];
		List -> List
	end,
	if Filter ->
			lists:filter(fun(Id) ->
				Id /= 0 andalso Id /= 16#FFFFFFFF
			end, Ids);
		not Filter -> Ids
	end.



% private
lookup(Tab, Id) ->
	case ets:lookup(Tab, Id) of
		[{Id, Record}] -> Record;
		[] -> none
	end.
