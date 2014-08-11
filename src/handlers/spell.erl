-module(spell).

-export([cast/1, cancel_cast/1]).

-include("include/binary.hrl").
-include("include/spell.hrl").
-include("include/database_records.hrl").


cast(Data) ->
	Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<SpellId?L, TargetMask?W, TargetsIn/binary>> = Payload,
	io:format("cast id ~p with mask ~p and targets ~p~n", [SpellId, TargetMask, TargetsIn]),
	Spell = static_store:lookup_spell(SpellId),
	io:format("spell: ~p~n", [Spell]),
	%io:format("spell effect: ~p~n", [Spell#spell_store.effect]),

	TargetInfo = spell_target_info:read(TargetMask, TargetsIn, Guid),
	%io:format("target info: ~p~n", [TargetInfo]),

	PackGuid = guid:pack(Guid),
	CastFlag = ?cast_flag_unknown9,
	{TargetsOut, NumTargets} = if TargetMask == ?target_flag_self -> {<<>>, 0};
		TargetMask == ?target_flag_unit -> {guid:unpack(TargetsIn), 1}
	end,
	OutPayload = <<PackGuid/binary, PackGuid/binary, SpellId?L, CastFlag?W, NumTargets?B, 0?B, TargetMask?W, TargetsOut/binary>>,
	OpAtom = smsg_spell_go,
	%world:send_to_all(OpAtom, OutPayload),


	CastFlagStart = ?cast_flag_unknown2,
	Timer = 2000,
	StartPayload = <<PackGuid/binary, PackGuid/binary, SpellId?L, CastFlagStart?W, Timer?L, TargetMask?W, TargetsIn/binary>>,
	StartOp = smsg_spell_start,
	world:send_to_all(StartOp, StartPayload),

	AccountId = recv_data:get(account_id, Data),
	ResultOp = smsg_cast_failed,
	ResultPayload = <<SpellId?L, 0?B>>,
	timer:apply_after(Timer, player_controller, send, [AccountId, ResultOp, ResultPayload]),
	timer:apply_after(Timer, world, send_to_all, [OpAtom, OutPayload]),

	ok.


cancel_cast(Data) ->
	<<SpellId?L>> = recv_data:get(payload, Data),
	io:format("cancel spell id: ~p~n", [SpellId]),
	Error = ?spell_failed_immune,
	{smsg_cast_failed, <<SpellId?L, Error?B>>}.
