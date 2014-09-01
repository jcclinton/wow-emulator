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

-module(item_data).

-export([init/0, cleanup/0]).
-export([store_values/1]).
-export([delete_items/1, delete_item/1]).
-export([get_values/1]).
-export([get_item_proto/1, get_item_id/1]).


init() ->
	dets_store:open(item_values, true),
	ok.

cleanup() ->
	dets_store:close(item_values, true),
	ok.



store_values(ItemValues) ->
	ItemGuid = item_values:get_guid(ItemValues),
	dets_store:store(item_values, {ItemGuid, ItemValues}, true).


get_values(ItemGuid) ->
	case dets_store:lookup(item_values, ItemGuid, true) of
		[] -> throw(badarg);
		[{ItemGuid, Values}] -> Values
	end.


delete_items(GuidList) ->
	lists:foreach(fun(Guid) ->
		delete_item(Guid)
	end, GuidList).

delete_item(Guid) ->
	dets_store:delete(item_values, Guid, true).


%% misc gets
get_item_proto(ItemGuid) ->
	ItemId = get_item_id(ItemGuid),
	content:lookup_item(ItemId).

get_item_id(ItemGuid) ->
	Values = get_values(ItemGuid),
	item_values:get_item_id(Values).
