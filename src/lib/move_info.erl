-module(move_info).

-export([write/1, read/1, update/3]).
-export([get_coords/1, get_value/2]).

-include("include/binary.hrl").
-include("include/movement.hrl").


update(Type, ValueIn, MoveData) ->
	lists:foldl(fun(Data={Key, _}, Acc) ->
		if Key == Type -> [{Type, ValueIn} | Acc];
			Key /= Type -> [Data|Acc]
		end
	end, [], MoveData).

get_coords(MoveData) ->
	[X, Y, Z, O] = get_values([x, y, z, o], MoveData),
	{X, Y, Z, O}.


get_value(Type, MoveData) ->
	proplists:get_value(Type, MoveData).



write(MoveData) ->
	{X, Y, Z, O} = get_coords(MoveData),
	MoveFlags = get_value(flags, MoveData),
	Time = get_value(time, MoveData),
	Bin = <<MoveFlags?L, Time?L, X?f, Y?f, Z?f, O?f>>,

	OnTaxi = util:has_flag(MoveFlags, ?moveflag_taxi),
	Bin2 = if OnTaxi ->
			[TGuid, TX, TY, TZ, TO, TTime] = get_values([tguid, tx, ty, tz, to, ttime], MoveData),
			<<Bin/binary, TGuid?f, TX?f, TY?f, TZ?f, TO?f, TTime?f>>;
		true -> Bin
	end,

	IsSwimming = util:has_flag(MoveFlags, ?moveflag_swimming),
	Bin3 = if IsSwimming ->
			[Pitch] = get_values([pitch], MoveData),
			<<Bin2/binary, Pitch?f>>;
		true -> Bin2
	end,
	
	% never sent when on taxi
	Bin4 = if not OnTaxi ->
			[FallTime] = get_values([fall_time], MoveData),
			<<Bin3/binary, FallTime?f>>;
		true -> Bin3
	end,

	IsFalling = util:has_flag(MoveFlags, ?moveflag_falling),
	IsRedirected = util:has_flag(MoveFlags, ?moveflag_redirected),
	Bin5 = if IsFalling orelse IsRedirected ->
			[Velocity, SinAngle, CosAngle, XYSpeed] = get_values([velocity, sin_angle, cos_angle, xy_speed], MoveData),
			<<Bin4/binary, Velocity?f, SinAngle?f, CosAngle?f, XYSpeed?f>>;
		true -> Bin4
	end,
	
	IsSplineMover = util:has_flag(MoveFlags, ?moveflag_spline_mover),
	Bin6 = if IsSplineMover ->
			[UUnk1] = get_values([u_unk1], MoveData),
			<<Bin5/binary, UUnk1?f>>;
		true -> Bin5
	end,

	Bin6.







read(Payload) ->
	<<MoveFlags?L, Time?L, X?f, Y?f, Z?f, O?f, Rest/binary>> = Payload,
	Values = [{flags, MoveFlags}, {time, Time}, {x, X}, {y, Y}, {z, Z}, {o, O}],
	MoveData = create(Values),

	OnTaxi = util:has_flag(MoveFlags, ?moveflag_taxi),
	{MoveData2, Rest2} = if OnTaxi ->
			% untested
			<<TGuid?Q, TX?f, TY?f, TZ?f, TO?f, TTime?L, R/binary>> = Rest,
			Data = [{tguid, TGuid}, {tx, TX}, {ty, TY}, {tz, TZ}, {to, TO}, {ttime, TTime}],
			DataOut = add(Data, MoveData),
			{DataOut, R};
		true -> {MoveData, Rest}
	end,

	IsSwimming = util:has_flag(MoveFlags, ?moveflag_swimming),
	{MoveData3, Rest3} = if IsSwimming ->
			% untested
			<<Pitch?f, R2/binary>> = Rest2,
			Data2 = [{pitch, Pitch}],
			DataOut2 = add(Data2, MoveData2),
			{DataOut2, R2};
		true -> {MoveData2, Rest2}
	end,
	
	% never sent when on taxi
	{MoveData4, Rest4} = if not OnTaxi ->
			% untested
			<<FallTime?f, R3/binary>> = Rest3,
			Data3 = [{fall_time, FallTime}],
			DataOut3 = add(Data3, MoveData3),
			{DataOut3, R3};
		true -> {MoveData3, Rest3}
	end,

	IsFalling = util:has_flag(MoveFlags, ?moveflag_falling),
	IsRedirected = util:has_flag(MoveFlags, ?moveflag_redirected),
	{MoveData5, Rest5} = if IsFalling orelse IsRedirected ->
			% untested
			<<Velocity?f, SinAngle?f, CosAngle?f, XYSpeed?f, R4/binary>> = Rest4,
			Data4 = [{velocity, Velocity}, {sin_angle, SinAngle}, {cos_angle, CosAngle}, {xy_speed, XYSpeed}],
			DataOut4 = add(Data4, MoveData4),
			{DataOut4, R4};
		true -> {MoveData4, Rest4}
	end,
	
	IsSplineMover = util:has_flag(MoveFlags, ?moveflag_spline_mover),
	{MoveData6, _Rest6} = if IsSplineMover ->
			% untested
			<<UUnk1?f, R5/binary>> = Rest5,
			Data5 = [{u_unk1, UUnk1}],
			DataOut5 = add(Data5, MoveData5),
			{DataOut5, R5};
		true -> {MoveData5, Rest5}
	end,


	MoveData6.







% private
get_values(Types, MoveData) ->
	Values = lists:foldl(fun(Type, Acc) ->
		Value = proplists:get_value(Type, MoveData),
		[Value|Acc]
	end, [], Types),
	lists:reverse(Values).

create(Values) ->
	lists:foldl(fun(Val, Acc) ->
		[Val|Acc]
	end, [], Values).
		

% adds values not previous in movedata
add(Values, MoveData) ->
	lists:foldl(fun(Val, Acc) ->
		[Val|Acc]
	end, MoveData, Values).
