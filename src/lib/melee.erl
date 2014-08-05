-module(melee).

-export([swing/1]).

-include("include/binary.hrl").
-include("include/attack.hrl").


swing(Guid) ->
	AttackOpAtom = smsg_attackerstateupdate,

	Values = char_data:get_values(Guid),
	MaxDamage = char_values:get(max_damage, Values),
	MinDamage = char_values:get(min_damage, Values),
	Damage = round(random:uniform() * (MaxDamage - MinDamage) + MinDamage),

	HitInfo = ?hitinfo_normalswing,
	PackGuid = guid:pack(Guid),
	TargetGuid = char_sess:get_target(Guid),
	TargetPackGuid = guid:pack(TargetGuid),
	DamageSchoolMask = 0,
	Absorb = 0,
	Resist = 0,
	TargetState = ?victimstate_normal,
	Blocked = 0,

	Payload = <<HitInfo?L, PackGuid/binary, TargetPackGuid/binary, Damage?L, 1?B, DamageSchoolMask?L, Damage?f, Damage?L, Absorb?L, Resist?L, TargetState?L, 0?L, 0?L, Blocked?L>>,
	world:send_to_all(AttackOpAtom, Payload),

	true.
