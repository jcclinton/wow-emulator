-module(recv_data).

-export([build/1, get/2]).


build(PropList) ->
	PropList.

get(Key, Data) ->
	Value = proplists:get_value(Key, Data),
	Value.
