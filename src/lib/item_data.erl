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
-export([get_item_proto/1]).
-export([set_guids/2]).

-include("include/data_types.hrl").


-spec init() -> 'ok'.
init() ->
	dets_store:open(item_values, true),
	ok.

-spec cleanup() -> 'ok'.
cleanup() ->
	dets_store:close(item_values, true),
	ok.



-spec store_values(item_values()) -> any().
store_values(ItemValues) ->
	ItemGuid = item_values:get_value(object_field_guid, ItemValues),
	dets_store:store(item_values, {ItemGuid, ItemValues}, true).


-spec get_values(guid()) -> item_values().
get_values(ItemGuid) ->
	case dets_store:lookup(item_values, ItemGuid, true) of
		[] -> throw(badarg);
		[{ItemGuid, Values}] -> Values
	end.


-spec delete_items([guid()]) -> any().
delete_items(GuidList) ->
	lists:foreach(fun(Guid) ->
		delete_item(Guid)
	end, GuidList).

-spec delete_item(guid()) -> any().
delete_item(Guid) ->
	dets_store:delete(item_values, Guid, true).


%% misc gets
-spec get_item_proto(guid() | item_values()) -> tuple().
get_item_proto(ItemGuid) when is_integer(ItemGuid) ->
	ItemValues = get_values(ItemGuid),
	get_item_proto(ItemValues);
get_item_proto(ItemValues) when is_binary(ItemValues) ->
	ItemId = item_value:get_value(object_field_entry, ItemValues),
	content:lookup_item(ItemId).

%% misc sets
-spec set_guids(guid(), guid()) -> 'ok'.
set_guids(ItemGuid, OwnerGuid) ->
	ItemValues = item_data:get_values(ItemGuid),
	NewItemValues1 = item_values:set_value(item_field_owner, OwnerGuid, ItemValues),
	NewItemValues = item_values:set_value(item_field_contained, OwnerGuid, NewItemValues1),
	item_data:store_values(NewItemValues),
	ok.
