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

-module(util).

-export([has_flag/2]).
-export([extract_string/1]).
-export([game_time/0, game_speed/0]).
-export([file_pread/3, file_open/2, file_close/1]).
-export([unreg_proc/2, reg_proc/2, get_pid/2, build_pid_key/2]).


-include("include/binary.hrl").



unreg_proc(Name, Id) ->
	Key = build_pid_key(Name, Id),
	gproc:unreg({n, l, Key}).

reg_proc(Name, Id) ->
	Key = build_pid_key(Name, Id),
	gproc:reg({n, l, Key}, none).

get_pid(Name, Id) ->
	Key = build_pid_key(Name, Id),
	gproc:lookup_pid({n, l, Key}).

build_pid_key(Name, Id) ->
	{Name, Id}.



has_flag(Flags, Flag) ->
	Flags band Flag /= 0.

% pulls out data until first zero byte
% then returns string plus rest of bin
extract_string(Bin) ->
	extract_string(Bin, <<>>).
extract_string(<<0?B, Rest/binary>>, Str) -> {Str, Rest};
extract_string(<<Char?B, Rest/binary>>, Str) ->
	extract_string(Rest, <<Str/binary, Char?B>>).


game_speed() ->
	0.01666667.

game_time() ->
    {Y, Mo, Dm} = erlang:date(),
    {H, Mi, _} = erlang:time(),
    Dw = calendar:day_of_the_week(Y, Mo, Dm),
    GameTime = (((((Mi band 16#3F) bor 
                   (H*64 band 16#7C0)) bor 
                   (Dw*2048 band 16#3800)) bor 
                   ((Dm - 1)*16384 band 16#FC000)) bor 
                   ((Mo - 1)*1048576 band 16#F00000)) bor 
                   ((Y - 2000)*16777216 band 16#1F000000),
    GameTime.



file_pread(Fd, Offset, Size) ->
	case file:pread(Fd, Offset, Size) of
		{error, Error} -> throw(Error);
		{ok, Result} -> Result
	end.

file_open(Filename, Options) ->
	case file:open(Filename, Options) of
		{error, Error} -> throw(Error);
		{ok, Fd} -> Fd
	end.

% wrapper around file:close to simplify code
file_close(Fd) ->
	case file:close(Fd) of
		{error, Error} -> throw(Error);
		ok -> ok
	end.
