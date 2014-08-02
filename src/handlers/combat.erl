-module(combat).

-export([attack_swing/1, attack_stop/1]).

-include("include/binary.hrl").


attack_swing(Data) ->
	Guid = recv_data:get(guid, Data),
	<<TargetGuid?Q>> = recv_data:get(payload, Data),
	io:format("attack swing~n"),

	StartPayload = <<Guid?Q, TargetGuid?Q>>,
	OpAtom = smsg_attackstart,
	world:send_to_all(OpAtom, StartPayload),
	ok.

	%error case
	%PackGuid = <<16#FF?B, Guid?Q>>,
	%TargetPackGuid = <<16#FF?B, TargetGuid?Q>>,
	%StopPayload = <<PackGuid/binary, TargetPackGuid/binary, 0?L>>,
	%{smsg_attackstop, StopPayload}.

attack_stop(_Data) ->
	% payload is empty
	io:format("attack stop~n"),
	ok.
