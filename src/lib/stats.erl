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

-module(stats).

-export([update_all/1]).

-include("include/items.hrl").
-include("include/binary.hrl").
-include("include/database_records.hrl").







% resets all item modification values and then resets them all
update_all(Guid) ->
	%io:format("UPDATING ALL~n"),
	ItemGuids = item:get_equipped_item_guids(Guid),

	NewPropList = lists:foldl(fun(ItemGuid, Acc) ->
		if ItemGuid > 0 ->
				%io:format("~nitem guid: ~p~n", [ItemGuid]),
				update_one(ItemGuid, Acc);
			ItemGuid == 0 -> []
		end
	end, get_empty_fields(), ItemGuids),

	player_state:set_multiple_values(Guid, NewPropList).




% sets all values for one item
% returns updated char values
update_one(ItemGuid, PrevPropList) ->
	ItemProto = item_data:get_item_proto(ItemGuid),
	IsEquippable = item:is_equippable(ItemProto),

	if IsEquippable ->
			Fields = get_fields(),
			lists:foldl(fun(Field, Acc) ->
				Value = get_item_value(Field, ItemProto),
				if Value > 0 ->
						CurrentValue = proplists:get_value(Field, Acc),
						NextAcc = proplists:delete(Field, Acc),
						NewValue = Value + CurrentValue,
						[{Field, NewValue} | NextAcc];
					Value == 0 -> Acc
				end
			end, PrevPropList, Fields);
		not IsEquippable -> []
	end.



%set all stats that are currently modifiable below


get_empty_fields() ->
	[{unit_field_resistances, 0}].

get_fields() ->
	[unit_field_resistances].

get_item_value(unit_field_resistances, ItemProto) -> ItemProto#item_proto.armor.
