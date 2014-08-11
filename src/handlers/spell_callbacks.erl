-module(spell_callbacks).

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

	spell:prepare(Guid, TargetInfo, Spell),
	ok.


cancel_cast(Data) ->
	<<SpellId?L>> = recv_data:get(payload, Data),
	io:format("cancel spell id: ~p~n", [SpellId]),
	Error = ?spell_failed_immune,
	{smsg_cast_failed, <<SpellId?L, Error?B>>}.
