-module(spell).

-export([cast/1, cancel_cast/1]).

-include("include/binary.hrl").
-include("include/spell.hrl").


cast(Data) ->
	Guid = recv_data:get(guid, Data),
	Payload = recv_data:get(payload, Data),
	<<SpellId?L, Targets/binary>> = Payload,
	io:format("cast id ~p with targets ~p~n", [SpellId, Targets]),
	PackGuid = <<16#FF?B, Guid?Q>>,
	CastFlag = ?cast_flag_unknown9,
	OutPayload = <<PackGuid/binary, PackGuid/binary, SpellId?L, CastFlag?W, 1?B, 0?B, Targets/binary>>,
	OpAtom = smsg_spell_go,
	world:send_to_all(OpAtom, OutPayload),
	Error = ?spell_failed_immune,
	{smsg_cast_failed, <<SpellId?L, 0?B>>}.


cancel_cast(Data) ->
	<<SpellId?L>> = recv_data:get(payload, Data),
	io:format("cancel spell id: ~p~n", [SpellId]),
	ok.
