-module(server).
-export([pong/1, null/1, accept_challenge/1, query_time/1]).

-include("include/binary.hrl").


query_time(_Data) ->
	Time = util:game_time(),
	Payload = <<Time?L>>,
	{smsg_query_time_response, Payload}.

pong(Data) ->
	Value = recv_data:get(payload, Data),
	<<Ping?L, _Latency?L>> = case Value of
		undefined -> throw(badarg);
		Value -> Value
	end,
	Payload = <<Ping?L>>,
	{smsg_pong, Payload}.


null(_Data) ->
	ok.

accept_challenge(Data) ->
	Payload = recv_data:get(payload, Data),
	% payload is created in rcv process and is passed straight through
	{smsg_auth_response, Payload}.
