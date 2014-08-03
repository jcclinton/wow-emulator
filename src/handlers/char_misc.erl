-module(char_misc).
-export([request_raid_info/1, name_query/1, cancel_trade/1, gmticket_getticket/1]).
-export([query_next_mail_time/1, battlefield_status/1, meetingstone_info/1, zone_update/1]).
-export([tutorial_flag/1, far_sight/1, set_selection/1, area_trigger/1]).
-export([set_sheathed/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


area_trigger(_Data) ->
	ok.

far_sight(Data) ->
	Payload = recv_data:get(payload, Data),
	% dont need to do anything
	io:format("received req to set far sight: ~p~n", [Payload]),
	% do nothing and camera stays on main char
	ok.

set_selection(Data) ->
	% TargetGuid is 0 when a target is deselected
	<<TargetGuid?Q>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	char_sess:store_target(Guid, TargetGuid),
	ok.

tutorial_flag(_Data) ->
	io:format("received req for tutorial flag~n"),
	ok.

zone_update(Data) ->
	<<Zone?L>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	CharMv = char_data:get_char_move(Guid),
	if CharMv#char_move.zone /= Zone ->
			NewCharMv = CharMv#char_move{zone=Zone},
			char_data:update_char_move(Guid, NewCharMv);
		true -> ok
	end,
	ok.

meetingstone_info(_Data) ->
	Payload = <<0?L, 6?B>>,
	{smsg_meetingstone_setqueue, Payload}.

battlefield_status(_Data) ->
	%io:format("received req for battlefield status~n"),
	ok.

query_next_mail_time(_Data) ->
	% reply with no unread mail
	Payload = <<16#C7A8C000?L, 0?L>>,
	{msg_query_next_mail_time, Payload}.

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
	Name = char_data:get_char_name(Guid),
	Null = <<"\0">>,
	Race = char_values:get(race, Guid),
	Gender = char_values:get(gender, Guid),
	Class = char_values:get(class, Guid),
	Payload = <<Guid?Q, Name/binary, Null/binary, Race?L, Gender?L, Class?L>>,
	{smsg_name_query_response, Payload}.

set_sheathed(Data) ->
	<<Value?L>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	Values = char_data:get_values(Guid),
	NewValues = char_values:set_sheathed(Value, Values),
	char_data:update_values(Guid, NewValues),

	ok.
