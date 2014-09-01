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

-module(account).
-export([create/2, init/0, destroy/0, lookup/1]).
-export([create_dummy_accounts/0]).

-include("include/database_records.hrl").

-define(acc, accounts).


init() ->
	dets_store:open(?acc),
	ok.

create_dummy_accounts() ->
	account:create("alice", "password123"),
	account:create("alice2", "password123").

destroy() ->
	dets_store:close(?acc),
	ok.



lookup(I) ->
	Result = dets_store:lookup(?acc, I),
	Value = case Result of
		[] -> false;
		[{_, Record}] -> Record
	end,
	Value.

create(Name, Password) when is_list(Name), is_list(Password), length(Name) > 0, length(Password) > 0 ->
	BinName = list_to_binary(Name),
	BinPassword = list_to_binary(Password),
	Salt = srp:generatePrivate(),
	DerivedKey = srp:getDerivedKey(BinName, BinPassword, Salt),
	Generator = srp:getGenerator(),
	Prime = srp:getPrime(),
	Verifier = srp:getVerifier(Generator, Prime, DerivedKey),
	Value = #account{name=BinName, salt=Salt, verifier=Verifier},
	Key = srp:normalize(BinName),
	%io:format("storing ~p~n", [Key]),
	Result = dets_store:store_new(?acc, {Key, Value}),
	if Result -> ok;
		not Result -> {error, name_taken}
	end;
create(_,_) ->
	throw(badarg).
