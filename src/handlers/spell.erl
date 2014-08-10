-module(spell).

-export([cast/1, cancel_cast/1]).

-include("include/binary.hrl").
-include("include/spell.hrl").
-include("include/database_records.hrl").


cast(Data) ->
	Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<SpellId?L, TargetMask?W, TargetsIn/binary>> = Payload,
	%io:format("cast id ~p with mask ~p and targets ~p~n", [SpellId, TargetMask, TargetsIn]),
	%Spell = static_store:lookup_spell(SpellId),
	%io:format("spell: ~p~n", [Spell]),
	%io:format("spell effect: ~p~n", [Spell#spell_store.effect]),

	PackGuid = guid:pack(Guid),
	CastFlag = ?cast_flag_unknown9,
	{TargetsOut, NumTargets} = if TargetMask == ?target_flag_self -> {<<>>, 0};
		TargetMask == ?target_flag_unit -> {guid:unpack(TargetsIn), 1}
	end,
	OutPayload = <<PackGuid/binary, PackGuid/binary, SpellId?L, CastFlag?W, NumTargets?B, 0?B, TargetMask?W, TargetsOut/binary>>,
	OpAtom = smsg_spell_go,
	world:send_to_all(OpAtom, OutPayload),

	{smsg_cast_failed, <<SpellId?L, 0?B>>}.


cancel_cast(Data) ->
	<<SpellId?L>> = recv_data:get(payload, Data),
	io:format("cancel spell id: ~p~n", [SpellId]),
	Error = ?spell_failed_immune,
	{smsg_cast_failed, <<SpellId?L, Error?B>>}.
