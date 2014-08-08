-module(melee).

-export([swing/2]).
-export([is_facing/3, within_distance/2]).

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



	TargetPackGuid = guid:pack(TargetGuid),
	DamageSchoolMask = 0,
	Absorb = 0,
	Resist = 0,
	TargetState = ?victimstate_normal,
	Blocked = 0,

	Payload = <<HitInfo?L, PackGuid/binary, TargetPackGuid/binary, Damage?L, 1?B, DamageSchoolMask?L, Damage?f, Damage?L, Absorb?L, Resist?L, TargetState?L, 0?L, 0?L, Blocked?L>>,
	world:send_to_all(AttackOpAtom, Payload),

	{true, NewSeed}.


is_facing(O, {X, Y, _}, {Tx, Ty, _}) ->
	Dx = Tx - X,
	Dy = Ty - Y,
	%offset is to align in-game orientation with atan2
	Offset = math:pi() / 2,
	Angle = math:atan2(Dy, Dx) + math:pi() + Offset,
	true.

within_distance({X, Y, Z}, {Tx, Ty, Tz}) ->
	Dx = Tx - X,
	Dy = Ty - Y,
	Dz = Tz - Z,
	DistSq = Dx * Dx + Dy * Dy + Dz * Dz,
	DistSq < ?base_meleerange_offset * ?base_meleerange_offset.
