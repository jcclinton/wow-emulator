-module(char_misc).
-export([request_raid_info/1, name_query/1, cancel_trade/1, gmticket_getticket/1]).
-export([query_next_mail_time/1, battlefield_status/1, meetingstone_info/1, zone_update/1]).
-export([tutorial_flag/1, far_sight/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


far_sight(PropList) ->
	Payload = proplists:get_value(payload, PropList),
	% dont need to do anything
	io:format("received req to set far sight: ~p~n", [Payload]),
	% do nothing and camera stays on main char
	ok.

tutorial_flag(_PropList) ->
	io:format("received req for tutorial flag~n"),
	ok.

zone_update(_PropList) ->
	io:format("received req for zone update~n"),
	ok.

meetingstone_info(_PropList) ->
	io:format("received req for meetingstone info~n"),
	ok.

battlefield_status(_PropList) ->
	io:format("received req for battlefield status~n"),
	ok.

query_next_mail_time(_PropList) ->
	io:format("received req to query next mail time~n"),
	ok.

gmticket_getticket(PropList) ->
	% send time response first
	ok = server:query_time(PropList),

	Opcode = opcode_patterns:getNumByAtom(smsg_gmticket_getticket),
	Msg = <<Opcode?W, 16#0A?L>>,
	player_controller:send(Msg),
	ok.

cancel_trade(_PropList) ->
	io:format("received req to cancel trade~n"),
	ok.

request_raid_info(_PropList) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_raid_instance_info),
	Msg = <<Opcode?W, 0?L>>,
	player_controller:send(Msg),
	ok.


name_query(PropList) ->
	Values = proplists:get_value(values, PropList),
	Guid = object_values:get_uint64_value('OBJECT_FIELD_GUID', Values),
	Name = char_data:get_logged_in_char_name(Guid),
	Opcode = opcode_patterns:getNumByAtom(smsg_name_query_response),
	Null = <<"\0">>,
	Race = object_values:get_byte_value('UNIT_FIELD_BYTES_0', Values, 0),
	Gender = object_values:get_byte_value('UNIT_FIELD_BYTES_0', Values, 1),
	Class = object_values:get_byte_value('UNIT_FIELD_BYTES_0', Values, 2),
	Msg = <<Opcode?W, Guid?Q, Name/binary, Null/binary, Race?L, Gender?L, Class?L>>,
	player_controller:send(Msg),
	ok.
