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

-module(world_crypto).

-export([encrypt/2, decrypt/2, encryption_key/1]).

-include("binary.hrl").

-define(K_SIZE, 40).

%% @spec encrypt(binary(), binary()) -> binary().
encrypt(Header, Key) ->
    encrypt(Header, Key, <<>>).

%% @spec encrypt(binary(), binary(), binary()) -> binary().
encrypt(<<>>, Key, Result) -> {Result, Key};
encrypt(<<OldByte?B, Header/binary>>, {SI, SJ, K}, Result) ->
    NewByte = ((lists:nth(SI+1, K) bxor OldByte) + SJ) band 255,
    NewSI   = (SI+1) rem ?K_SIZE,
    encrypt(Header, {NewSI, NewByte, K}, <<Result/binary, NewByte:8>>).

%% @spec decrypt(binary(), binary()) -> binary().
decrypt(Header, Key) ->
    decrypt(Header, Key, <<>>).

%% @spec decrypt(binary(), binary(), binary()) -> binary().
decrypt(<<>>, Key, Result) -> {Result, Key};
decrypt(<<OldByte?B, Header/binary>>, {RI, RJ, K}, Result) ->
    NewByte = (lists:nth(RI+1, K) bxor (OldByte - RJ)) band 255,
    NewRI   = (RI + 1) rem ?K_SIZE,
    decrypt(Header, {NewRI, OldByte, K}, <<Result/binary, NewByte:8>>).

%% @spec encrypt(string()) -> list().
encryption_key(AccountId) ->
	Key = char_sess:get_session_key(AccountId),
	binary_to_list(Key).
