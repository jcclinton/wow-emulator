-module(char_misc).
-export([request_raid_info/1, name_query/1, cancel_trade/1, gmticket_getticket/1]).
-export([query_next_mail_time/1, battlefield_status/1, meetingstone_info/1, zone_update/1]).
-export([tutorial_flag/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


tutorial_flag(PropList) ->
	io:format("received req for tutorial flag~n"),
	ok.

zone_update(PropList) ->
	io:format("received req for zone update~n"),
	ok.

meetingstone_info(PropList) ->
	io:format("received req for meetingstone info~n"),
	ok.

battlefield_status(PropList) ->
	io:format("received req for battlefield status~n"),
	ok.

query_next_mail_time(PropList) ->
	io:format("received req to query next mail time~n"),
	ok.

gmticket_getticket(PropList) ->
	io:format("received req to get gmticket~n"),
	ok.

cancel_trade(PropList) ->
	io:format("received req to cancel trade~n"),
	ok.

request_raid_info(PropList) ->
	io:format("received req for raid info~n"),
	ok.


name_query(PropList) ->
	io:format("received req for name query~n"),
	ok.
