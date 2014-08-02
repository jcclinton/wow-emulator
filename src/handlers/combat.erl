-module(combat).

-export([attack_swing/1, attack_stop/1]).

-include("include/binary.hrl").


attack_swing(Data) ->
	<<TargetGuid?Q>> = recv_data:get(payload, Data),
	io:format("attack swing~n"),
	ok.

attack_stop(_Data) ->
	% payload is empty
	io:format("attack stop~n"),
	ok.
