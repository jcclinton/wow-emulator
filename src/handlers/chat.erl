-module(chat).
-export([join_channel/2, message_chat/2]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


message_chat(_PropList, _AccountId) ->
	%io:format("received req to message chat~n"),
	ok.

join_channel(_PropList, _accountId) ->
	%io:format("received req to join channel~n"),
	ok.

