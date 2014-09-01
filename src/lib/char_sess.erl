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

-module(char_sess).

-export([init/0, cleanup/0]).
-export([create/1, delete/1]).
-export([store_connected_client/2, get_session_key/1]).


-include("include/database_records.hrl").

% used to store ephemeral session data
-define(sess, character_session).

% used to store connected client session keys
-define(conn, connected_clients).


init() ->
	ets:new(?conn, [named_table, set, public]),
	ets:new(?sess, [named_table, set, public]).

cleanup() ->
	ets:delete(?conn),
	ets:delete(?sess).


% char session data

create(Guid) ->
	ets:insert(?sess, {Guid, #char_sess{}}).

delete(Guid) ->
	ets:delete(?sess, Guid).



% authorized connection data

store_connected_client(AccountId, Key) ->
	ets:insert(?conn, {AccountId, Key}).

get_session_key(AccountId) ->
	% for now, just crash if this client is authed
	[{AccountId, Key}] = ets:lookup(?conn, AccountId),
	% remove from ets
	% no point in keeping it around
	ets:delete(?conn, AccountId),
	Key.

