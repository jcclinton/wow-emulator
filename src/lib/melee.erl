-module(melee).

-export([swing/2]).
-export([is_facing/3, within_distance/2]).

-include("include/database_records.hrl").
-include("include/binary.hrl").
-include("include/attack.hrl").


swing(Guid, Seed) ->
	AttackOpAtom = smsg_attackerstateupdate,

	Values = char_data:get_values(Guid),
	MaxDamage = char_values:get(max_damage, Values),
	MinDamage = char_values:get(min_damage, Values),
	{Rand, NewSeed} = random:uniform_s(Seed),
	Damage = round(Rand * (MaxDamage - MinDamage) + MinDamage),

	HitInfo = ?hitinfo_normalswing,
	PackGuid = guid:pack(Guid),
	TargetGuid = char_sess:get_target(Guid),

	TargetCharMove = char_data:get_char_move(TargetGuid),
	Tx = TargetCharMove#char_move.x,
	Ty = TargetCharMove#char_move.y,
	Tz = TargetCharMove#char_move.z,
	CharMove = char_data:get_char_move(Guid),
	X = CharMove#char_move.x,
	Y = CharMove#char_move.y,
	Z = CharMove#char_move.z,
	CharVect = {X, Y, Z},
	TargetVect = {Tx, Ty, Tz},
	O = CharMove#char_move.orient,

	IsFacing = is_facing(O, CharVect, TargetVect),
	IsInDistance = within_distance(CharVect, TargetVect),



	%io:format("is facing: ~p is in distance: ~p~n", [IsFacing, IsInDistance]),
	if IsFacing andalso IsInDistance ->
			TargetPackGuid = guid:pack(TargetGuid),
			DamageSchoolMask = 0,
			Absorb = 0,
			Resist = 0,
			TargetState = ?victimstate_normal,
			Blocked = 0,

			Payload = <<HitInfo?L, PackGuid/binary, TargetPackGuid/binary, Damage?L, 1?B, DamageSchoolMask?L, Damage?f, Damage?L, Absorb?L, Resist?L, TargetState?L, 0?L, 0?L, Blocked?L>>,
			world:send_to_all(AttackOpAtom, Payload),
			ok;
		true -> ok
	end,

	{true, NewSeed}.




is_facing(O, {X, Y, _}, {Tx, Ty, _}) ->
	Dx = Tx - X,
	Dy = Ty - Y,

	% offset is to line up atan2 with 0 radians pointing north
	% which is how in-game orientation works
	% orientation gets larger as it rotates left
	%Offset = math:pi() / 2,
	RawAngle = math:atan2(Dy, Dx),
	TwoPi = 2 * math:pi(),

	% angle is bound between 0 and 2 pi
	Angle = if RawAngle < 0 -> RawAngle + TwoPi;
		true -> RawAngle
	end,
				%io:format("o: ~p dx: ~.2f dy: ~.2f angle: ~.2f~n", [O, Dx, Dy, Angle]),

	% 120 degree arc
	Arc = TwoPi / 6,
	BoundBot = O - Arc,
	BoundTop = O + Arc,
	LowerBound = if BoundBot < 0 -> BoundBot + TwoPi;
		true -> BoundBot
	end,
	UpperBound = if BoundTop > TwoPi -> BoundTop - TwoPi;
		true -> BoundTop
	end,


	% first handle case where upper bound has gone past 2pi but lower bound has not
	Result = if LowerBound > UpperBound ->
			% if orientation has not gone past 2pi, it will be facing if its bigger than lower bound
			% if orientation has gone past 2pi, it will be facing if its smaller than upper bound
			Angle > LowerBound orelse Angle < UpperBound;
		LowerBound < UpperBound ->
			Angle > LowerBound andalso Angle < UpperBound
	end,
	io:format("o: ~.2f lower: ~.2f upper: ~.2f angle: ~.2f result: ~p~n", [O, LowerBound, UpperBound, Angle, Result]),
	Result.

within_distance({X, Y, Z}, {Tx, Ty, Tz}) ->
	Dx = Tx - X,
	Dy = Ty - Y,
	Dz = Tz - Z,
	DistSq = Dx * Dx + Dy * Dy + Dz * Dz,
	% base_meleerange_offset is too close for some reason
	Dist = ?base_meleerange_offset * 2,
	ReqDistSq = Dist * Dist,
	%io:format("distsq: ~p req distsq: ~p~n", [DistSq, ReqDistSq]),
	DistSq < ReqDistSq.
