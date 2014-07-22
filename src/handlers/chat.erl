-module(chat).
-export([join_channel/1, message_chat/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


message_chat(_Data) ->
	%io:format("received req to message chat~n"),
	ok.

join_channel(_Data) ->
	%io:format("received req to join channel~n"),
	ok.

