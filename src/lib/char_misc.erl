-module(char_misc).
-export([request_raid_info/1, name_query/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


request_raid_info(PropList) ->
	io:format("received req for raid info~n"),
	ok.


name_query(PropList) ->
	io:format("received req for name query~n"),
	ok.
