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

-module(spell).

-export([prepare/3]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/spell.hrl").
-include("include/database_records.hrl").
-include("include/data_types.hrl").


-spec prepare(guid(), dict:dict(atom(), number()), tuple()) -> 'ok'.
prepare(CasterGuid, TargetInfo, Spell) ->
	send_spell_start(CasterGuid, TargetInfo, Spell),

	timer:apply_after(2000, spell, send_spell_result, [CasterGuid, TargetInfo, Spell]),
	timer:apply_after(2000, spell, send_spell_go, [CasterGuid, TargetInfo, Spell]),

	ok.


-spec send_spell_result(guid(), dict:dict(atom(), number()), tuple()) -> 'ok'.
send_spell_result(CasterGuid, _TargetInfo, Spell) ->
	io:format("spell result~n"),

	SpellId = Spell#spell_store.id,
	ResultOp = smsg_cast_failed,
	ResultPayload = <<SpellId?L, 0?B>>,

	AccountId = char_data:get_account_id(CasterGuid),
	player_controller:send(AccountId, ResultOp, ResultPayload),

	ok.


-spec send_spell_start(guid(), dict:dict(atom(), number()), tuple()) -> 'ok'.
send_spell_start(CasterGuid, TargetInfo, Spell) ->
	io:format("spell start~n"),
	PackGuid = guid:pack(CasterGuid),
	TargetMask = spell_target_info:lookup(target_mask, TargetInfo),
	SpellId = Spell#spell_store.id,
	CastFlagStart = ?cast_flag_unknown2,
	Timer = 2000,

	TargetData = if TargetMask == ?target_flag_unit ->
			TargetGuid = spell_target_info:lookup(target_guid, TargetInfo),
			guid:pack(TargetGuid);
		true -> <<>>
	end,

	StartPayload = <<PackGuid/binary, PackGuid/binary, SpellId?L, CastFlagStart?W, Timer?L, TargetMask?W, TargetData/binary>>,
	StartOp = smsg_spell_start,
	world:send_to_all(StartOp, StartPayload),
	ok.


-spec send_spell_go(guid(), dict:dict(atom(), number()), tuple()) -> 'ok'.
send_spell_go(CasterGuid, TargetInfo, Spell) ->
	io:format("spell go~n"),
	TargetMask = spell_target_info:lookup(target_mask, TargetInfo),
	PackGuid = guid:pack(CasterGuid),
	CastFlag = ?cast_flag_unknown9,
	TargetGuid = spell_target_info:lookup(target_guid, TargetInfo),

	{TargetsOut, HitTargets, NumTargets} = if TargetMask == ?target_flag_self ->
			spell_aura:add(CasterGuid, Spell),
			{<<>>, <<>>, 0};
		TargetMask == ?target_flag_unit ->
			TargetGuidBin = guid:int_to_bin(TargetGuid),
			{TargetGuidBin, TargetGuidBin, 1}
	end,

	SpellId = Spell#spell_store.id,
	OutPayload = <<PackGuid/binary, PackGuid/binary, SpellId?L, CastFlag?W, NumTargets?B, HitTargets/binary, 0?B, TargetMask?W, TargetsOut/binary>>,
	OpAtom = smsg_spell_go,
	world:send_to_all(OpAtom, OutPayload),
	ok.
