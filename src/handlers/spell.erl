-module(spell).

-export([cast/1, cancel_cast/1]).

-include("include/binary.hrl").
-include("include/spell.hrl").


cast(Data) ->
	Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<SpellId?L, TargetMask?W, Targets/binary>> = Payload,
	io:format("cast id ~p with mask ~p and targets ~p~n", [SpellId, TargetMask, Targets]),

	PackGuid = guid:pack(Guid),
	CastFlag = ?cast_flag_unknown9,
	NumTargets = 0,
	OutPayload = <<PackGuid/binary, PackGuid/binary, SpellId?L, CastFlag?W, NumTargets?B, 0?B, TargetMask?W>>,
	OpAtom = smsg_spell_go,
	world:send_to_all(OpAtom, OutPayload),

	{smsg_cast_failed, <<SpellId?L, 0?B>>}.


cancel_cast(Data) ->
	<<SpellId?L>> = recv_data:get(payload, Data),
	io:format("cancel spell id: ~p~n", [SpellId]),
	Error = ?spell_failed_immune,
	{smsg_cast_failed, <<SpellId?L, Error?B>>}.
