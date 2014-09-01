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

-module(server).
-export([pong/1, null/1, accept_challenge/1, query_time/1]).

-include("include/binary.hrl").


query_time(_Data) ->
	Time = util:game_time(),
	Payload = <<Time?L>>,
	{smsg_query_time_response, Payload}.

pong(Data) ->
	Value = recv_data:get(payload, Data),
	<<Ping?L, _Latency?L>> = case Value of
		undefined -> throw(badarg);
		Value -> Value
	end,
	Payload = <<Ping?L>>,
	{smsg_pong, Payload}.


null(_Data) ->
	ok.

accept_challenge(Data) ->
	Payload = recv_data:get(payload, Data),
	% payload is created in rcv process and is passed straight through
	{smsg_auth_response, Payload}.
