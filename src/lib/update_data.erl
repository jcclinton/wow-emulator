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

-module(update_data).

-export([compress/1, decompress/1]).
-export([build_create_update_packet_for_player/2, build_update_packet/2]).
-export([build_create_update_packet_for_items/1]).

-include("include/database_records.hrl").
-include("include/binary.hrl").
-include("include/movement.hrl").
-include("include/updates.hrl").
-include("include/types.hrl").
-include("include/data_types.hrl").


-type maybe_tuple() :: tuple() | 'none'.


-spec build_create_update_packet_for_player(guid(), boolean()) -> term().
build_create_update_packet_for_player(Guid, IsSelf) ->
	CharMove = char_data:get_char_move(Guid),
	ItemGuidList = item:get_item_guids(Guid),
	ItemTypeId = ?typeid_item,
	ItemUpdateFlag = ?updateflag_all,
	{ItemBlocks, ItemBlockCount} = lists:foldl(fun(ItemGuid, {Blocks, Count}) ->
		if ItemGuid > 0 ->
				ItemValues = item_data:get_values(ItemGuid),
				Block = create_block(CharMove, ItemValues, false, ItemTypeId, ItemUpdateFlag, ItemGuid),
				NewBlocks = <<Blocks/binary, Block/binary>>,
				{NewBlocks, Count+1};
			true ->
				{Blocks, Count}
		end
	end, {<<>>, 0}, ItemGuidList),

	PlayerTypeId = ?typeid_player,
	PlayerUpdateFlags = ?updateflag_living bor ?updateflag_all bor ?updateflag_has_position,
	Values = player_state:get_values(Guid),
	PlayerBlock = create_block(CharMove, Values, IsSelf, PlayerTypeId, PlayerUpdateFlags, Guid),
	TotalCount = ItemBlockCount + 1,
	build_packet(<<ItemBlocks/binary, PlayerBlock/binary>>, TotalCount).



-spec build_create_update_packet_for_items([guid()]) -> term().
build_create_update_packet_for_items(ItemGuids) ->
	ItemTypeId = ?typeid_item,
	ItemUpdateFlag = ?updateflag_all,
	{Blocks, TotalCount} = lists:foldl(fun(ItemGuid, {Acc, Count}) ->
		ItemValues = item_data:get_values(ItemGuid),
		Block = create_block(none, ItemValues, false, ItemTypeId, ItemUpdateFlag, ItemGuid),
		{<<Acc/binary, Block/binary>>, Count+1}
	end, {<<>>, 0}, ItemGuids),
	build_packet(Blocks, TotalCount).



-spec build_update_packet(binary(), player_values()) -> term().
build_update_packet(Mask, Values) ->
	Blocks = update_block(Mask, Values),
	BlockCount = 1,
	build_packet(Blocks, BlockCount).

-spec build_packet(binary(), non_neg_integer()) -> handler_response().
build_packet(Blocks, BlockCount) ->
	HasTransport = 0,
	Payload = <<BlockCount?L, HasTransport?B, Blocks/binary>>,
	PayloadSize = byte_size(Payload),
	if PayloadSize > 100 ->
			CompressedPayload = update_data:compress(Payload),
			{smsg_compressed_update_object, <<PayloadSize?L, CompressedPayload/binary>>};
		true ->
			{smsg_update_object, Payload}
	end.



-spec update_block(binary(), player_values()) -> binary().
update_block(Mask, Values) ->
	Guid = char_values:get_value(object_field_guid, Values),
	UpdateType = ?updatetype_values,
	PackedGuid = guid:pack(Guid),
	ValuesCount = (byte_size(Values) div 4) - 1,
	Blocks = (ValuesCount + 31) div 32,
	ValuesData = build_values_update(Mask, Values, ValuesCount),

	<<UpdateType?B, PackedGuid/binary, Blocks?B, Mask/binary, ValuesData/binary>>.



-spec create_block(maybe_tuple(), any_values(), boolean(), non_neg_integer(), non_neg_integer(), guid()) -> binary().
create_block(CharMove, Values, IsSelf, TypeId, UpdateFlag, Guid) ->
	UpdateType = if IsSelf -> ?updatetype_create_object2;
		not IsSelf -> ?updatetype_create_object
	end,
	PackedGuid = guid:pack(Guid),
	%Guid = <<7, 41, 179, 24>>,
%io:format("update binary guid: ~p~n", [Guid]),

	MovementData = if CharMove /= none ->
			getMovementData(CharMove, IsSelf, UpdateFlag);
		CharMove == none -> <<>>
	end,
	ValuesCount = (byte_size(Values) div 4) - 1,
	Blocks = (ValuesCount + 31) div 32,
	EmptyMaskBits = update_mask:empty(ValuesCount),
	MaskBits = update_mask:set_bits(ValuesCount, EmptyMaskBits, Values),
	%io:format("maskbits: ~p~n", [MaskBits]),
	ValuesData = build_values_update(MaskBits, Values, ValuesCount),
	%io:format("value bits: ~p~n", [ValuesData]),

	<<UpdateType?B, PackedGuid/binary, TypeId?B, MovementData/binary, Blocks?B, MaskBits/binary, ValuesData/binary>>.



-spec build_values_update(binary(), player_values(), non_neg_integer()) -> binary().
build_values_update(MaskBits, Values, Count) ->
	TypeFlags = object_values:get_int32_value(object_field_type, Values),
	IsUnit = util:has_flag(TypeFlags, ?typemask_unit),
	%IsUnit = false,
	IsPlayer = util:has_flag(TypeFlags, ?typemask_player),
	lists:foldl(fun(Index, Bin) ->
		BitFlag = update_mask:get_bit(MaskBits, Index),
		if BitFlag ->
				if IsUnit ->
						IsNonNegFloat = is_non_neg_float_field(Index),
						IsFloat = is_float_field(Index),
						if IsNonNegFloat ->
								build_non_neg_float(Index, Values, Bin);
							IsPlayer andalso IsFloat ->
								build_float(Index, Values, Bin);
							true ->
								build_uint32(Index, Values, Bin)
						end;
					not IsUnit ->
						build_uint32(Index, Values, Bin)
				end;
			true -> Bin
		end
	end, <<>>, lists:seq(0, Count)).


-spec is_non_neg_float_field(non_neg_integer()) -> boolean().
is_non_neg_float_field(Index) ->
	Fields = [
		unit_field_baseattacktime,
		unit_field_offhandattacktime,
		unit_field_rangedattacktime
	],
	lists:foldl(fun(Field, Bool) ->
		if Bool -> Bool;
			not Bool ->
				FieldIndex = object_fields:fields(Field),
				if FieldIndex == Index -> true;
					FieldIndex /= Index -> Bool
				end
		end
	end, false, Fields).
		

-spec is_float_field(non_neg_integer()) -> boolean().
is_float_field(Index) ->
	Fields = [
		player_field_posstat0,
		player_field_posstat1,
		player_field_posstat2,
		player_field_posstat3,
		player_field_posstat4,
		player_field_negstat0,
		player_field_negstat1,
		player_field_negstat2,
		player_field_negstat3,
		player_field_negstat4
	],

	IsFloat = lists:foldl(fun(Field, Bool) ->
		if Bool -> Bool;
			not Bool ->
				FieldIndex = object_fields:fields(Field),
				if FieldIndex == Index -> true;
					FieldIndex /= Index -> Bool
				end
		end
	end, false, Fields),

	if IsFloat -> IsFloat;
		not IsFloat ->
			PosIndex = object_fields:fields(player_field_resistancebuffmodspositive),
			NegIndex = object_fields:fields(player_field_resistancebuffmodsnegative),
			if Index >= PosIndex andalso Index =< (PosIndex + 6) -> true;
				Index >= NegIndex andalso Index =< (NegIndex + 6) -> true;
				true -> false
			end
	end.
		

% adds uint32 number to acc
-spec build_uint32(non_neg_integer(), player_values(), binary()) -> binary().
build_uint32(Index, Values, Bin) ->
	Value = object_values:get_uint32_value(Index, Values),
	<<Bin/binary, Value?L>>.

% converts float to uint32
-spec build_float(non_neg_integer(), player_values(), binary()) -> binary().
build_float(Index, Values, Bin) ->
	Value = object_values:get_float_value(Index, Values),
	UintValue = round(Value),
	<<Bin/binary, UintValue?L>>.

% converts positive float to uint32
-spec build_non_neg_float(non_neg_integer(), player_values(), binary()) -> binary().
build_non_neg_float(Index, Values, Bin) ->
	RawValue = object_values:get_float_value(Index, Values),
	Value = if RawValue < 0 -> 0;
		true -> round(RawValue)
	end,
	<<Bin/binary, Value?L>>.






-spec getMovementData(tuple(), boolean(), non_neg_integer()) -> binary().
getMovementData(CharMove, IsSelf, Flags) ->
	Self = ?updateflag_self,
	UpdateFlags = if IsSelf -> Flags bor Self;
		not IsSelf -> Flags
	end,

	FunList = [
		fun(Acc) ->
			HasFlag = util:has_flag(UpdateFlags, ?updateflag_living),
			if HasFlag ->
					MoveFlags = ?moveflag_move_stop,
					WorldTime = util:game_time(),
					<<Acc/binary, MoveFlags?L, WorldTime?L>>;
				not HasFlag -> Acc
			end
		end,

		fun(Acc) ->
			HasFlag = util:has_flag(UpdateFlags, ?updateflag_has_position),
			if HasFlag ->
					X = CharMove#char_move.x,
					Y = CharMove#char_move.y,
					Z = CharMove#char_move.z,
					O = CharMove#char_move.orient,
					<<Acc/binary, X?f, Y?f, Z?f, O?f>>;
				not HasFlag -> Acc
			end
		end,

		fun(Acc) ->
			HasFlag = util:has_flag(UpdateFlags, ?updateflag_living),
			if HasFlag ->
					{W, R, WB, S, SB, _F, _FB, T, _P} = {2.5, 7, 4.5, 4.72, 2.5, 7, 4.5, 3.141593, 1.0},
					<<Acc/binary, 0?f, W?f, R?f, WB?f, S?f, SB?f, T?f>>;
				not HasFlag -> Acc
			end
		end,

		fun(Acc) ->
			HasFlag = util:has_flag(UpdateFlags, ?updateflag_all),
			if HasFlag -> <<Acc/binary, 1?L>>;
				not HasFlag -> Acc
			end
		end
	],

	lists:foldl(fun(Fun, Bin) ->
		Fun(Bin)
	end, <<UpdateFlags?B>>, FunList).




-spec decompress(binary()) -> binary().
decompress(Packet) ->
	Z = zlib:open(),
	zlib:inflateInit(Z),
	Data = zlib:inflate(Z, Packet),
	zlib:inflateEnd(Z),
	list_to_binary(Data).

-spec compress(binary()) -> binary().
compress(Packet) ->
    Z  = zlib:open(),
    ok = zlib:deflateInit(Z, best_speed),
    P  = zlib:deflate(Z, Packet, finish),
    ok = zlib:deflateEnd(Z),
    list_to_binary(P).

