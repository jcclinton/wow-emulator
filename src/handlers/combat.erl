-module(combat).

-export([attack_swing/1, attack_stop/1]).

-include("include/binary.hrl").


attack_swing(Data) ->
	Guid = recv_data:get(guid, Data),
	<<TargetGuid?Q>> = recv_data:get(payload, Data),
	io:format("start attack~n"),

	StartPayload = <<Guid?Q, TargetGuid?Q>>,
	OpAtom = smsg_attackstart,
	world:send_to_all(OpAtom, StartPayload),

	unit_melee:start_melee_attack(Guid),
	ok.


attack_stop(Data) ->
	% payload is empty
	Guid = recv_data:get(guid, Data),
	unit_melee:stop_melee_attack(Guid),
	io:format("stop attack~n"),

	% normally this is when a fight ends,
	% or on an error
	% but for now just end it when the client stops auto-attacking
	PackGuid = guid:pack(Guid),
	TargetGuid = char_sess:get_target(Guid),
	TargetPackGuid = guid:pack(TargetGuid),
	StopPayload = <<PackGuid/binary, TargetPackGuid/binary, 0?L>>,
	{smsg_attackstop, StopPayload}.
