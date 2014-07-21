-module(server).
-export([pong/2, null/2, accept_challenge/2, query_time/2]).

-include("include/binary.hrl").


query_time(_Data, AccountId) ->
	Time = util:game_time(),
	Payload = <<Time?L>>,
	player_router:send(AccountId, smsg_query_time_response, Payload),
	ok.

pong(Data, AccountId) ->
	Value = recv_data:get(payload, Data),
	<<Ping?L, _Latency?L>> = case Value of
		undefined -> throw(badarg);
		Value -> Value
	end,
	Payload = <<Ping?L>>,
	player_router:send(AccountId, smsg_pong, Payload),
	ok.


null(_Data, _AccountId) ->
	ok.

accept_challenge(Data, AccountId) ->
	Payload = recv_data:get(payload, Data),
	% payload is created in rcv process and is passed straight through
	player_router:send(AccountId, smsg_auth_response, Payload),
	ok.
