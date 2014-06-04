-module(update_data).

-export([compress/1, decompress/1, block/2]).

-include("include/database_records.hrl").
-include("include/binary.hrl").


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



	block(Char, Values) ->
		UpdateType = 3, %char_create2
		%Guid = Char#char.id,
		Guid = <<7,41,179,24>>,
		TypeId = 4, %type player
		MovementData = getMovementData(Char),
		ValuesCount = (byte_size(Values) div 4) - 1,
		EmptyMaskBits = update_mask:empty(ValuesCount),
		MaskBits = update_mask:set_bits(ValuesCount, EmptyMaskBits, Values),
		%io:format("maskbits: ~p~n", [MaskBits]),
		ValuesData = build_values_update(MaskBits, Values, ValuesCount),
		%io:format("value bits: ~p~n", [ValuesData]),

		<<UpdateType?B, Guid/binary, TypeId?B, MovementData/binary, ValuesData/binary>>.


	build_values_update(MaskBits, Values, Count) ->
		Blocks = (Count + 31) div 32,
		ValuesData = lists:foldl(fun(Index, Bin) ->
			BitFlag = update_mask:get_bit(MaskBits, Index),
			if BitFlag ->
								Value = object_values:get_value(Index, Values),
								<<Bin/binary, Value?L>>;
							true -> Bin
						end
		end, <<>>, lists:seq(0, Count)),
		<<Blocks?B, MaskBits/binary, ValuesData/binary>>.







	getMovementData(Char) ->
		All = 16#10,
		Self = 16#01,
		Living = 16#20,
		HasPosition = 16#40,
		UpdateFlags = All bor Self bor Living bor HasPosition,
		MoveFlags = 0,
		WorldTime = 1000,
        Speeds         = {2.5, 7, 4.5, 4.72, 2.5,
                          7, 4.5, 3.141593, 1.0},
    {W, R, WB, S, SB, _F, _FB, T, _P} = Speeds,
		X = Char#char.position_x,
		Y = Char#char.position_y,
		Z = Char#char.position_z,
		O = Char#char.orientation,
		<<UpdateFlags?B, MoveFlags?L, WorldTime?L, X?f, Y?f, Z?f, O?f, 0?f, W?f, R?f, WB?f, S?f, SB?f, T?f, 1?L>>.
