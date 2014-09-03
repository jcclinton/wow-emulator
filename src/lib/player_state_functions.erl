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
-export([apply_aura/4]).
-export([set_item/2, set_visible_item/2]).
-export([get_item_guid/2, get_item_guids/1]).

-include("include/character.hrl").
-include("include/items.hrl").
-include("include/binary.hrl").


% gets

get_first_empty_inv_slot(Values) ->
	FirstSlot = ?inventory_slot_item_start,
	get_first_empty_inv_slot(Values, FirstSlot).

get_first_empty_inv_slot(_, ?inventory_slot_item_end) -> -1;
get_first_empty_inv_slot(Values, Slot) ->
	Index = object_fields:fields(player_field_inv_slot_head) + (2 * Slot),
	SlotValue = object_values:get_uint64_value(Index, Values),

	if SlotValue == 0 -> Slot;
		SlotValue > 0 ->
			get_first_empty_inv_slot(Values, Slot + 1)
	end.


get_item_guid(Values, Slot) ->
	SlotGuids = get_item_guids(Values),
	Offset = 8 * Slot,
	<<_:Offset/binary, SlotGuid?Q, _/binary>> = SlotGuids,
	SlotGuid.

get_item_guids(Values) ->
	Index = object_fields:fields(player_field_inv_slot_head) * 4,
	% inventory_slot_item_end starts at 0
	Len = (?inventory_slot_item_end + 1) * 8,
	binary:part(Values, Index, Len).





% sets


take_damage(Values, Damage) ->
	OldAmount = char_values:get_value(unit_field_health, Values),
	NewAmount = if OldAmount >= Damage ->
			OldAmount - Damage;
		OldAmount < Damage -> 0
	end,
	char_values:set_value(unit_field_health, NewAmount, Values).


apply_aura(Values, Slot, SpellId, Level) ->
	Funs = [
		{add_aura, {Slot, SpellId}},
		{add_aura_flag, Slot},
		{add_aura_level, {Slot, Level}},
		{add_aura_application, Slot}
	],
	M = spell_aura,
	lists:foldl(fun({F, Arg}, {ValuesAcc, Indices}) ->
		{NewValues, NewIndices} = M:F(Arg, ValuesAcc),
		{NewValues, NewIndices ++ Indices}
	end, {Values, []}, Funs).


set_item(Values, {Slot, ItemGuid}) ->
	Field = player_field_inv_slot_head,
	Offset = 2 * Slot,
	Index = object_fields:fields(Field) + Offset,
	% this can set an item from equipped, bags, bag inventory, bank and keyrings
	NextIndex = object_fields:fields(player_farsight),
	if Index >= NextIndex orelse Slot < 0 -> throw(badarg);
		true -> ok
	end,
	char_values:set_value({Field, Offset}, ItemGuid, Values).


set_visible_item(Values, {Slot, ItemId}) ->
	Field = player_visible_item_1_0,
	Offset = Slot * ?max_visible_item_offset,
	Index = object_fields:fields(Field) + Offset,
	NextIndex = object_fields:fields(player_field_inv_slot_head),
	% this can only be used to set equipped items
	if Index >= NextIndex orelse Slot < 0 -> throw(badarg);
		true -> ok
	end,
	%io:format("tryin gto set VISUAL ~p to ~p with offset ~p~n", [ItemId, Field, Offset]),
	char_values:set_value({Field, Offset}, ItemId, Values).
