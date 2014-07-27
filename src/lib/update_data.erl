-module(update_data).

-export([compress/1, decompress/1]).
-export([build_create_packet/1, build_update_packet/2]).

-include("include/database_records.hrl").
-include("include/binary.hrl").
-include("include/movement.hrl").
-include("include/updates.hrl").
-include("include/types.hrl").





build_update_packet(Mask, Values) ->
	Block = update_block(Mask, Values),
	BlockCount = 1,
	build_packet(Block, BlockCount).

build_create_packet({CharMove, Values, IsSelf}) ->
	Block = create_block(CharMove, Values, IsSelf),
	BlockCount = 1,
	build_packet(Block, BlockCount).

build_packet(Block, BlockCount) ->
	HasTransport = 0,
	Payload = <<BlockCount?L, HasTransport?B, Block/binary>>,
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



create_block(CharMove, Values, IsSelf) ->
	UpdateType = if IsSelf -> ?updatetype_create_object2;
		not IsSelf -> ?updatetype_create_object
	end,
	Guid = char_values:get(guid, Values),
	PackedGuid = pack_guid(Guid),
	%Guid = <<7, 41, 179, 24>>,
%io:format("update binary guid: ~p~n", [Guid]),

	TypeId = ?typeid_player,
	MovementData = getMovementData(CharMove, IsSelf),
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



		% 7 tells you the size of the guid
		% eg for a 3 byte guid,
		% 7 = (1 << 0) bor (1 << 1) bor (1 << 2)
		% players are always 3 bytes
		% objects can be a maximum up to 8 bytes
		%GuidInt2 = GuidInt + 1,
pack_guid(Guid) ->
	<<7?B, Guid?G>>.




getMovementData(CharMove, IsSelf) ->
	All = ?updateflag_all,
	Self = ?updateflag_self,
	Living = ?updateflag_living,
	HasPosition = ?updateflag_has_position,
	Flags = All bor Living bor HasPosition,
	UpdateFlags = if IsSelf -> Flags bor Self;
		not IsSelf -> Flags
	end,
	MoveFlags = ?moveflag_move_stop,
	WorldTime = util:game_time(),

	{W, R, WB, S, SB, _F, _FB, T, _P} = {2.5, 7, 4.5, 4.72, 2.5, 7, 4.5, 3.141593, 1.0},

	X = CharMove#char_move.x,
	Y = CharMove#char_move.y,
	Z = CharMove#char_move.z,
	O = CharMove#char_move.orient,
	<<UpdateFlags?B, MoveFlags?L, WorldTime?L, X?f, Y?f, Z?f, O?f, 0?f, W?f, R?f, WB?f, S?f, SB?f, T?f, 1?L>>.



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

