-module(update_data).

-export([compress/1, decompress/1, block/3]).

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



	block(Char, Values, IsSelf) ->
		UpdateType = if IsSelf ->
				3; %char_create2
			not IsSelf ->
				2 %char_create
		end,
		GuidInt = Char#char.id,
		% 7 tells you the size of the guid
		% eg for a 3 byte guid,
		% 7 = (1 << 0) bor (1 << 1) bor (1 << 2)
		% players are always 3 bytes
		% objects can be a maximum up to 8 bytes
		%GuidInt2 = GuidInt + 1,
		Guid = <<7, GuidInt?G>>,
		%Guid = <<7, 41, 179, 24>>,
	io:format("update binary guid: ~p~n", [Guid]),

		TypeId = 4, %type player
		MovementData = getMovementData(Char, IsSelf),
		ValuesCount = (byte_size(Values) div 4) - 1,
		Blocks = (ValuesCount + 31) div 32,
		EmptyMaskBits = update_mask:empty(ValuesCount),
		MaskBits = update_mask:set_bits(ValuesCount, EmptyMaskBits, Values),
		%io:format("maskbits: ~p~n", [MaskBits]),
		ValuesData = build_values_update(MaskBits, Values, ValuesCount),
		%io:format("value bits: ~p~n", [ValuesData]),

		<<UpdateType?B, Guid/binary, TypeId?B, MovementData/binary, Blocks?B, MaskBits/binary, ValuesData/binary>>.


	build_values_update(MaskBits, Values, Count) ->
		lists:foldl(fun(Index, Bin) ->
			BitFlag = update_mask:get_bit(MaskBits, Index),
			if BitFlag ->
								Value = object_values:get_value(Index, Values),
								<<Bin/binary, Value?L>>;
							true -> Bin
						end
		end, <<>>, lists:seq(0, Count)).







	getMovementData(Char, IsSelf) ->
		All = 16#10,
		Self = 16#01,
		Living = 16#20,
		HasPosition = 16#40,
		Flags = All bor Living bor HasPosition,
		UpdateFlags = if IsSelf -> Flags bor Self;
			not IsSelf -> Flags
		end,
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
