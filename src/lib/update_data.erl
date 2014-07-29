-module(update_data).

-export([compress/1, decompress/1]).
-export([build_create_update_packet_for_player/2, build_update_packet/2]).

-include("include/database_records.hrl").
-include("include/binary.hrl").
-include("include/movement.hrl").
-include("include/updates.hrl").
-include("include/types.hrl").



build_create_update_packet_for_player(Guid, IsSelf) ->
	CharMove = char_data:get_char_move(Guid),
	ItemGuidList = item:get_equipped_item_guids(Guid),
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
	Values = char_data:get_values(Guid),
	PlayerBlock = create_block(CharMove, Values, IsSelf, PlayerTypeId, PlayerUpdateFlags, Guid),
	TotalCount = ItemBlockCount + 1,
	build_packet(<<ItemBlocks/binary, PlayerBlock/binary>>, TotalCount).



build_update_packet(Mask, Values) ->
	Blocks = update_block(Mask, Values),
	BlockCount = 1,
	build_packet(Blocks, BlockCount).

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



update_block(Mask, Values) ->
	Guid = char_values:get(guid, Values),
	UpdateType = ?updatetype_values,
	PackedGuid = pack_guid(Guid),
	ValuesCount = (byte_size(Values) div 4) - 1,
	Blocks = (ValuesCount + 31) div 32,
	ValuesData = build_values_update(Mask, Values, ValuesCount),

	<<UpdateType?B, PackedGuid/binary, Blocks?B, Mask/binary, ValuesData/binary>>.



create_block(CharMove, Values, IsSelf, TypeId, UpdateFlag, Guid) ->
	UpdateType = if IsSelf -> ?updatetype_create_object2;
		not IsSelf -> ?updatetype_create_object
	end,
	PackedGuid = pack_guid(Guid),
	%Guid = <<7, 41, 179, 24>>,
%io:format("update binary guid: ~p~n", [Guid]),

	MovementData = getMovementData(CharMove, IsSelf, UpdateFlag),
	ValuesCount = (byte_size(Values) div 4) - 1,
	Blocks = (ValuesCount + 31) div 32,
	EmptyMaskBits = update_mask:empty(ValuesCount),
	MaskBits = update_mask:set_bits(ValuesCount, EmptyMaskBits, Values),
	%io:format("maskbits: ~p~n", [MaskBits]),
	ValuesData = build_values_update(MaskBits, Values, ValuesCount),
	%io:format("value bits: ~p~n", [ValuesData]),

	<<UpdateType?B, PackedGuid/binary, TypeId?B, MovementData/binary, Blocks?B, MaskBits/binary, ValuesData/binary>>.



build_values_update(MaskBits, Values, Count) ->
	lists:foldl(fun(Index, Bin) ->
		BitFlag = update_mask:get_bit(MaskBits, Index),
		if BitFlag ->
							Value = object_values:get_value(Index, Values),
							<<Bin/binary, Value?L>>;
						true -> Bin
					end
	end, <<>>, lists:seq(0, Count)).



		% first byte is a mask that tells you the size of the guid
		% eg for a 3 byte guid,
		% 7 = (1 << 0) bor (1 << 1) bor (1 << 2)
		% objects can be a maximum up to 8 bytes
pack_guid(Guid) ->
	%<<7?B, Guid?G>>.
	<<16#FF?B, Guid?Q>>.




getMovementData(CharMove, IsSelf, Flags) ->
	Self = ?updateflag_self,
	UpdateFlags = if IsSelf -> Flags bor Self;
		not IsSelf -> Flags
	end,
	MoveFlags = ?moveflag_move_stop,
	WorldTime = util:game_time(),
	X = CharMove#char_move.x,
	Y = CharMove#char_move.y,
	Z = CharMove#char_move.z,
	O = CharMove#char_move.orient,
	{W, R, WB, S, SB, _F, _FB, T, _P} = {2.5, 7, 4.5, 4.72, 2.5, 7, 4.5, 3.141593, 1.0},

	FunList = [
		fun(Acc) ->
			HasFlag = util:has_flag(UpdateFlags, ?updateflag_living),
			if HasFlag -> <<Acc/binary, MoveFlags?L, WorldTime?L>>;
				not HasFlag -> Acc
			end
		end,

		fun(Acc) ->
			HasFlag = util:has_flag(UpdateFlags, ?updateflag_has_position),
			if HasFlag -> <<Acc/binary, X?f, Y?f, Z?f, O?f>>;
				not HasFlag -> Acc
			end
		end,

		fun(Acc) ->
			HasFlag = util:has_flag(UpdateFlags, ?updateflag_living),
			if HasFlag -> <<Acc/binary, 0?f, W?f, R?f, WB?f, S?f, SB?f, T?f>>;
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

	%<<UpdateFlags?B, MoveFlags?L, WorldTime?L, X?f, Y?f, Z?f, O?f, 0?f, W?f, R?f, WB?f, S?f, SB?f, T?f, 1?L>>.



decompress(Packet) ->
	Z = zlib:open(),
	zlib:inflateInit(Z),
	Data = zlib:inflate(Z, Packet),
	zlib:inflateEnd(Z),
	list_to_binary(Data).

compress(Packet) ->
    Z  = zlib:open(),
    ok = zlib:deflateInit(Z, best_speed),
    P  = zlib:deflate(Z, Packet, finish),
    ok = zlib:deflateEnd(Z),
    list_to_binary(P).

