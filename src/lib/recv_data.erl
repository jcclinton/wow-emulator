-module(recv_data).

-export([build/1, get/2, add_value/3]).


build(PropList) ->
	PropList.

get(Key, Data) ->
	Value = proplists:get_value(Key, Data),
	Value.

add_value(Data, Key, Value) ->
	[{Key, Value} | Data].
