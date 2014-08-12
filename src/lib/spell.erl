-module(spell).

-export([prepare/3]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/spell.hrl").
-include("include/database_records.hrl").


prepare(CasterGuid, TargetInfo, Spell) ->
	send_spell_start(CasterGuid, TargetInfo, Spell),

	timer:apply_after(2000, spell, send_spell_result, [CasterGuid, TargetInfo, Spell]),
	timer:apply_after(2000, spell, send_spell_go, [CasterGuid, TargetInfo, Spell]),

	ok.


send_spell_result(CasterGuid, _TargetInfo, Spell) ->
	io:format("spell result~n"),

	SpellId = Spell#spell_store.id,
	ResultOp = smsg_cast_failed,
	ResultPayload = <<SpellId?L, 0?B>>,

	AccountId = char_data:get_account_id(CasterGuid),
	player_controller:send(AccountId, ResultOp, ResultPayload),

	ok.


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
