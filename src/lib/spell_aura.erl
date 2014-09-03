%   This is a World of Warcraft emulator written in erlang, supporting
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

-module(spell_aura).

-export([add/2]).
-export([set_aura_duration/2]).
-export([add_aura/2, add_aura_flag/2, add_aura_level/2, add_aura_application/2]).

-include("include/binary.hrl").
-include("include/spell.hrl").
-include("include/database_records.hrl").


add(Guid, Spell) ->
	SpellId = Spell#spell_store.id,
	Slot = 12,
	Level = 1,

	player_state:run_async_function(Guid, apply_aura, [Slot, SpellId, Level]),


	timer:apply_after(100, spell_aura, set_aura_duration, [Guid, Slot]).


set_aura_duration(Guid, Slot) ->
	OpAtom = smsg_update_aura_duration,
	Time = 1000 * 60 * 30,
	Payload = <<Slot?B, Time?L>>,
	AccountId = char_data:get_account_id(Guid),
	player_controller:send(AccountId, OpAtom, Payload),
	ok.




add_aura({Slot, SpellId}, Values) ->
	Field = unit_field_aura,
	Index = object_fields:fields(Field) + Slot,
	NextIndex = object_fields:fields(unit_field_aura_last),
	if Index >= NextIndex orelse Slot < 0 -> throw(badarg);
		true -> ok
	end,
	char_values:set_value({Field, Slot}, SpellId, Values).

add_aura_flag(Slot, Values) ->
	SlotIndex = Slot bsr 3,
	Field = unit_field_auraflags,
	Index = object_fields:fields(Field) + SlotIndex,
	Flags = object_values:get_uint32_value(Index, Values),
	Byte = (Slot band 7) bsl 2,
	FlagMask = 9,
	NewFlags = Flags bor (FlagMask bsl Byte),
	char_values:set_value({Field, SlotIndex}, NewFlags, Values).

add_aura_level({Slot, Level}, Values) ->
	SlotIndex = Slot div 4,
	Byte = (Slot rem 4) * 8,
	Field = unit_field_auralevels,
	Index = object_fields:fields(Field) + SlotIndex,
	OldLevels = object_values:get_uint32_value(Index, Values),

	Tmp = OldLevels band (bnot (16#FF bsl Byte)),
	NewLevels = Tmp bor (Level bsl Byte),

	char_values:set_value({Field, SlotIndex}, NewLevels, Values).

add_aura_application(Slot, Values) ->
	SlotIndex = Slot div 4,
	Byte = (Slot rem 4) * 8,
	Field = unit_field_auraapplications,
	Index = object_fields:fields(Field) + SlotIndex,
	OldApp = object_values:get_uint32_value(Index, Values),

	NewApp = OldApp bor (0 bsl Byte),

	char_values:set_value({Field, SlotIndex}, NewApp, Values).
