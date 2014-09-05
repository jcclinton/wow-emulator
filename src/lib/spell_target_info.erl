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

-module(spell_target_info).

-export([lookup/2]).
-export([read/3]).

-include("include/binary.hrl").
-include("include/spell.hrl").
-include("include/database_records.hrl").
-include("include/data_types.hrl").

-spec lookup(atom(), dict:dict(atom(), number())) -> number().
lookup(Key, TargetInfo) ->
	case dict:find(Key, TargetInfo) of
		{ok, Value} -> Value;
		Res -> Res
	end.


-spec read(non_neg_integer(), binary(), guid()) -> dict:dict(atom(), number()).
read(TargetMask, TargetData, CasterGuid) ->
	Funs = [
		{?target_flag_self,
			fun(Data) ->
				CharMove = char_data:get_char_move(CasterGuid),
				X = CharMove#char_move.x,
				Y = CharMove#char_move.y,
				Z = CharMove#char_move.z,
				ListOut = [
					{dest_x, X},
					{dest_y, Y},
					{dest_z, Z},
					{target_guid, CasterGuid}
				],
				{Data, ListOut}
			end},
		{?target_flag_unit bor ?target_flag_unk2,
			fun(Data) ->
				{GuidBin, Rest} = guid:extract_packed(Data),
				Guid = guid:bin_to_int(GuidBin),
				ListOut = [
					{target_guid, Guid}
				],
				{Rest, ListOut}
			end},
		{?target_flag_object bor ?target_flag_object_unk,
			fun(Data) ->
				{GuidBin, Rest} = guid:extract_packed(Data),
				Guid = guid:bin_to_int(GuidBin),
				ListOut = [
					%game object
					{go_target_guid, Guid}
				],
				{Rest, ListOut}
			end},
		{?target_flag_item bor ?target_flag_trade_item,
			fun(Data) ->
				{GuidBin, Rest} = guid:extract_packed(Data),
				Guid = guid:bin_to_int(GuidBin),
				ListOut = [
					{item_target_guid, Guid}
				],
				{Rest, ListOut}
			end},
		{?target_flag_source_location,
			fun(Data) ->
				<<SrcX?f, SrcY?f, SrcZ?f, Rest/binary>> = Data,
				ListOut = [
					{src_x, SrcX},
					{src_y, SrcY},
					{src_z, SrcZ}
				],
				{Rest, ListOut}
			end},
		{?target_flag_dest_location,
			fun(Data) ->
				<<DestX?f, DestY?f, DestZ?f, Rest/binary>> = Data,
				ListOut = [
					{dest_x, DestX},
					{dest_y, DestY},
					{dest_z, DestZ}
				],
				{Rest, ListOut}
			end},
		{?target_flag_string,
			fun(Data) ->
				{Str, Rest} = util:extract_string(Data),
				ListOut = [
					{str_target, Str}
				],
				{Rest, ListOut}
			end},
		{?target_flag_corpse bor ?target_flag_pvp_corpse,
			fun(Data) ->
				{GuidBin, Rest} = guid:extract_packed(Data),
				Guid = guid:bin_to_int(GuidBin),
				ListOut = [
					{corpse_target_guid, Guid}
				],
				{Rest, ListOut}
			end}
	],

	{_, DictOut} = lists:foldl(fun({Flag, Fun}, {DataIn, Dict}) ->
		HasFlag = util:has_flag(TargetMask, Flag) or (Flag == 0 andalso TargetMask == 0),
		if HasFlag ->
				{DataNext, ListOut} = Fun(DataIn),
				DictNext = lists:foldl(fun({Key, Value}, DictAcc) ->
					dict:store(Key, Value, DictAcc)
				end, Dict, ListOut),
				{DataNext, DictNext};
			not HasFlag ->
				{DataIn, Dict}
		end
	end, {TargetData, dict:new()}, Funs),

	dict:store(target_mask, TargetMask, DictOut).
