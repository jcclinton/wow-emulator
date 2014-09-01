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

-module(player_state_functions).

-export([get_first_empty_inv_slot/1]).
-export([take_damage/2]).

-include("include/character.hrl").


get_first_empty_inv_slot(Values) ->
	FirstSlot = ?inventory_slot_item_start,
	get_first_empty_inv_slot(Values, FirstSlot).

get_first_empty_inv_slot(_, ?inventory_slot_item_end) -> -1;
get_first_empty_inv_slot(Values, Slot) ->
	SlotValue = char_values:get(item, {Slot, Values}),

	if SlotValue == 0 -> Slot;
		SlotValue > 0 ->
			get_first_empty_inv_slot(Values, Slot + 1)
	end.




take_damage(Values, Damage) ->
	OldAmount = char_values:get(health, Values),
	NewAmount = if OldAmount >= Damage ->
			OldAmount - Damage;
		OldAmount < Damage -> 0
	end,
	char_values:set(health, NewAmount, Values).
