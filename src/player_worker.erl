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

-module(player_worker).

-export([start_link/1, call/1]).

-include("include/shared_defines.hrl").


start_link(WorkerData) ->
	Pid = spawn_link(?MODULE, call, [WorkerData]),
	{ok, Pid}.



% used to call callback functions
% if a callback returns ok, nothing happens
% if it returns {OpAtom, Payload}
% it sends that packet to this player
call({Callback, Args}) ->
	Data = recv_data:build(Args),
	M = Callback#callback.module,
	F = Callback#callback.function,

	case M:F(Data) of
		ok -> ok;
		{OpAtom, Payload} ->

			Type = case Callback#callback.send_priority of
				undefined -> enqueue;
				Prio -> Prio
			end,

			AccountId = recv_data:get(account_id, Data),
			player_controller:send(AccountId, OpAtom, Payload, Type)
	end.
