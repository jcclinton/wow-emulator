-module(server).
-export([pong/1, null/1, accept_challenge/1, query_time/1]).

-include("include/binary.hrl").


query_time(_PropList) ->
	Time = util:game_time(),
	Opcode = opcode_patterns:getNumByAtom(smsg_query_time_response),
	Msg = <<Opcode?W, Time?L>>,
	player_controller:send(Msg),
	ok.

pong(PropList) ->
	Value = proplists:get_value(payload, PropList),
	<<Ping?L, _Latency?L>> = case Value of
		undefined -> throw(badarg);
		Value -> Value
	end,
	Opcode = opcode_patterns:getNumByAtom(smsg_pong),
	Msg = <<Opcode?W, Ping?L>>,
	player_controller:send(Msg),
	ok.


null(_PropList) ->
	%Opcode = opcode_patterns:getNumByAtom(msg_null_action),
	%Msg = <<Opcode?W>>,
	%player_controller:send(Msg),
	ok.

accept_challenge(PropList) ->
	Payload = proplists:get_value(payload, PropList),
	Opcode = opcode_patterns:getNumByAtom(smsg_auth_response),
	% payload is created in rcv process and is passed straight through
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.