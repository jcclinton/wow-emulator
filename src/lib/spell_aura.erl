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
-compile([export_all]).

-include("include/binary.hrl").
-include("include/spell.hrl").
-include("include/database_records.hrl").


add(Guid, Spell) ->
	SpellId = Spell#spell_store.id,
	Slot = 12,
	Level = 1,

	PropList = [
		{aura, {Slot, SpellId}},
		{aura_flag, Slot},
		{aura_level, {Slot, Level}},
		{aura_application, Slot}
	],
	player_state:set_multiple_values(Guid, PropList),


	timer:apply_after(100, spell_aura, set_aura_duration, [Guid, Slot]).


set_aura_duration(Guid, Slot) ->
	OpAtom = smsg_update_aura_duration,
	Time = 1000 * 60 * 30,
	Payload = <<Slot?B, Time?L>>,
	AccountId = char_data:get_account_id(Guid),
	player_controller:send(AccountId, OpAtom, Payload),
	ok.
