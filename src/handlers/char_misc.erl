-module(char_misc).
-export([request_raid_info/1, name_query/1, cancel_trade/1, gmticket_getticket/1]).
-export([query_next_mail_time/1, battlefield_status/1, meetingstone_info/1, zone_update/1]).
-export([tutorial_flag/1, far_sight/1, set_selection/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


far_sight(Data) ->
	Payload = recv_data:get(payload, Data),
	% dont need to do anything
	io:format("received req to set far sight: ~p~n", [Payload]),
	% do nothing and camera stays on main char
	ok.

set_selection(Data) ->
	<<TargetGuid?Q>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	char_data:store_selection(Guid, TargetGuid),
	ok.

tutorial_flag(_Data) ->
	io:format("received req for tutorial flag~n"),
	ok.

zone_update(Data) ->
	Payload = recv_data:get(payload, Data),
	io:format("received req for zone update: ~p~n", [Payload]),
	ok.

meetingstone_info(_Data) ->
	io:format("received req for meetingstone info~n"),
	ok.

battlefield_status(_Data) ->
	io:format("received req for battlefield status~n"),
	ok.

query_next_mail_time(_Data) ->
	io:format("received req to query next mail time~n"),
	ok.

gmticket_getticket(Data) ->
	% send time response first
	AccountId = recv_data:get(account_id, Data),
	{OpAtom, Payload1} = server:query_time(Data),
	player_controller:send(AccountId, OpAtom, Payload1),

	Payload = <<16#0A?L>>,
	{smsg_gmticket_getticket, Payload}.

cancel_trade(_Data) ->
	%io:format("received req to cancel trade~n"),
	ok.

request_raid_info(_Data) ->
	Payload = <<0?L>>,
	{smsg_raid_instance_info, Payload}.


name_query(Data) ->
	<<Guid?Q>> = recv_data:get(payload, Data),
	CharMisc = char_data:get_char_misc(Guid),
	Name = CharMisc#char_misc.name,
	Values = char_data:get_values(Guid),
	Null = <<"\0">>,
	Race = object_values:get_byte_value('UNIT_FIELD_BYTES_0', Values, 0),
	Gender = object_values:get_byte_value('UNIT_FIELD_BYTES_0', Values, 1),
	Class = object_values:get_byte_value('UNIT_FIELD_BYTES_0', Values, 2),
	Payload = <<Guid?Q, Name/binary, Null/binary, Race?L, Gender?L, Class?L>>,
	{smsg_name_query_response, Payload}.
