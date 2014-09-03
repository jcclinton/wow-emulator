%%   This is a World of Warcraft emulator written in erlang, supporting
%%   client 1.12.x
%%
%%   Copyright (C) 2014  Jamie Clinton <jamieclinton.com>
%%
%%   This program is free software; you can redistribute it and/or modify
%%   it under the terms of the GNU General Public License as published by
%%   the Free Software Foundation; either version 2 of the License, or
%%   (at your option) any later version.
%%
%%   This program is distributed in the hope that it will be useful,
%%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%   GNU General Public License for more details.
%%
%%   You should have received a copy of the GNU General Public License along
%%   with this program; if not, write to the Free Software Foundation, Inc.,
%%   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%
%%   World of Warcraft, and all World of Warcraft or Warcraft art, images,
%%   and lore ande copyrighted by Blizzard Entertainment, Inc.

-module(misc).
-export([request_raid_info/1, name_query/1, cancel_trade/1, gmticket_getticket/1]).
-export([query_next_mail_time/1, battlefield_status/1, meetingstone_info/1, zone_update/1]).
-export([tutorial_flag/1, far_sight/1, set_selection/1, area_trigger/1]).
-export([set_sheathed/1, repop_request/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


repop_request(_Data) ->
	% sent when the player dies
	io:format("cmsg repop request~n"),
	ok.

area_trigger(_Data) ->
	% not needed
	% used when character enters different areas
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
	player_state:set_value(Guid, TargetGuid, unit_field_target),
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

	% race, class, gender
	RaceKey = {unit_field_bytes_0, 0},
	GenderKey = {unit_field_bytes_0, 1},
	ClassKey = {unit_field_bytes_0, 2},

	Fields = [RaceKey, GenderKey, ClassKey],
	ValuesData = player_state:get_values(Guid, Fields),

	Race = proplists:get_value(RaceKey, ValuesData),
	Gender = proplists:get_value(GenderKey, ValuesData),
	Class = proplists:get_value(ClassKey, ValuesData),

	Payload = <<Guid?Q, Name/binary, 0?B, Race?L, Gender?L, Class?L>>,
	{smsg_name_query_response, Payload}.


set_sheathed(Data) ->
	<<Sheathed?L>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),

	player_state:set_value(Guid, Sheathed, {unit_field_bytes_2, 0}),

	ok.
