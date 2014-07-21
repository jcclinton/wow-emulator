-module(server).
-export([pong/1, null/1, accept_challenge/1, query_time/1]).

-include("include/binary.hrl").


query_time(_PropList) ->
	Time = util:game_time(),
	Payload = <<Time?L>>,
	player_controller:send(smsg_query_time_response, Payload),
	ok.

pong(PropList) ->
	Value = proplists:get_value(payload, PropList),
	<<Ping?L, _Latency?L>> = case Value of
		undefined -> throw(badarg);
		Value -> Value
	end,
	Payload = <<Ping?L>>,
	player_controller:send(smsg_pong, Payload),
	ok.


null(_PropList) ->
	%player_controller:send(msg_null_action, <<>>),
	ok.

accept_challenge(PropList) ->
	Payload = proplists:get_value(payload, PropList),
	% payload is created in rcv process and is passed straight through
	player_controller:send(smsg_auth_response, Payload),
	ok.
