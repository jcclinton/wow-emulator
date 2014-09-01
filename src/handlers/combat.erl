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

-module(combat).

-export([attack_swing/1, attack_stop/1]).

-include("include/binary.hrl").


attack_swing(Data) ->
	Guid = recv_data:get(guid, Data),
	<<TargetGuid?Q>> = recv_data:get(payload, Data),
	io:format("start attack~n"),

	StartPayload = <<Guid?Q, TargetGuid?Q>>,
	OpAtom = smsg_attackstart,
	world:send_to_all(OpAtom, StartPayload),

	unit_melee:start_melee_attack(Guid),
	ok.


attack_stop(Data) ->
	% payload is empty
	Guid = recv_data:get(guid, Data),
	unit_melee:stop_melee_attack(Guid),
	io:format("stop attack~n"),

	% normally this is when a fight ends,
	% or on an error
	% but for now just end it when the client stops auto-attacking
	PackGuid = guid:pack(Guid),
	TargetGuid = player_state:get_value(Guid, target),
	TargetPackGuid = guid:pack(TargetGuid),
	StopPayload = <<PackGuid/binary, TargetPackGuid/binary, 0?L>>,
	{smsg_attackstop, StopPayload}.
