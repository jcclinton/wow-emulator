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

-module(melee).

-export([swing/2]).
-export([is_facing/3, within_distance/2]).
-export([generate_attack_table/0, roll/2]).
-compile([export_all]).

-include("include/database_records.hrl").
-include("include/binary.hrl").
-include("include/attack.hrl").

swing(Guid, Seed) ->
	TargetGuid = player_state:get_value(Guid, unit_field_target),

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
	NewSeed = if IsFacing andalso IsInDistance ->
			AttackOpAtom = smsg_attackerstateupdate,

			Fields = [unit_field_maxdamage, unit_field_mindamage],
			ValuesData = player_state:get_values(Guid, Fields),

			MaxDamage = proplists:get_value(unit_field_maxdamage, ValuesData),
			MinDamage = proplists:get_value(unit_field_mindamage, ValuesData),

			{Rand, NewSeed1} = rand:uniform_s(Seed),
			Damage = round(Rand * (MaxDamage - MinDamage) + MinDamage),
			%io:format("max ~p min ~p dam ~p rand ~p~n", [MaxDamage, MinDamage, Damage, Rand]),

			PackGuid = guid:pack(Guid),
			TargetPackGuid = guid:pack(TargetGuid),

			HitInfo = ?hitinfo_normalswing,
			DamageSchoolMask = 0,
			Absorb = 0,
			Resist = 0,
			TargetState = ?victimstate_normal,
			Blocked = 0,

			player_state:run_async_function(TargetGuid, take_damage, [Damage]),

			Payload = <<HitInfo?L, PackGuid/binary, TargetPackGuid/binary, Damage?L, 1?B, DamageSchoolMask?L, Damage?f, Damage?L, Absorb?L, Resist?L, TargetState?L, 0?L, 0?L, Blocked?L>>,
			world:send_to_all(AttackOpAtom, Payload),
			NewSeed1;
		true -> Seed
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
    Angle =
        if
            RawAngle < 0 -> RawAngle + TwoPi;
            true -> RawAngle
        end,
    %io:format("o: ~p dx: ~.2f dy: ~.2f angle: ~.2f~n", [O, Dx, Dy, Angle]),

    % 120 degree arc
    Arc = TwoPi / 6,
    BoundBot = O - Arc,
    BoundTop = O + Arc,
    LowerBound =
        if
            BoundBot < 0 -> BoundBot + TwoPi;
            true -> BoundBot
        end,
    UpperBound =
        if
            BoundTop > TwoPi -> BoundTop - TwoPi;
            true -> BoundTop
        end,

    % first handle case where upper bound has gone past 2pi but lower bound has not
    Result =
        if
            LowerBound > UpperBound ->
                % if orientation has not gone past 2pi, it will be facing if its bigger than lower bound
                % if orientation has gone past 2pi, it will be facing if its smaller than upper bound
                Angle > LowerBound orelse Angle < UpperBound;
            LowerBound < UpperBound ->
                Angle > LowerBound andalso Angle < UpperBound
        end,
    %io:format("o: ~.2f lower: ~.2f upper: ~.2f angle: ~.2f result: ~p~n", [O, LowerBound, UpperBound, Angle, Result]),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% test attack table code below

-define(miss, 0).
-define(dodge, 1).
-define(parry, 2).
-define(glancing_blow, 3).
-define(block, 4).
-define(critical_hit, 5).
-define(crushing_blow, 6).
-define(ordinary_hit, 7).

generate_attack_table() ->
    Attacks = [
        {?miss, 5.02},
        {?dodge, 4.50},
        {?parry, 6.23},
        {?glancing_blow, 0.00},
        {?block, 5.10},
        {?critical_hit, 5.08},
        {?crushing_blow, 0.07},
        {?ordinary_hit, -1}
    ],

    TotalSize = get_size(),
    {Table, _} = lists:foldl(
        fun({Type, Amount}, {Tab, LastAmount}) ->
            NewAmount =
                if
                    Amount >= 0 ->
                        round(Amount * (get_size() / 100));
                    Amount == -1 ->
                        TotalSize - LastAmount
                end,
            io:format("~p: ~p~n", [Type, LastAmount]),

            Rest = binary:copy(<<Type ?B>>, NewAmount),
            {<<Tab/binary, Rest/binary>>, LastAmount + NewAmount}
        end,
        {<<>>, 0},
        Attacks
    ),
    io:format("~p: ~p~n", [t, TotalSize]),
    Table.

get_size() ->
    10000.

test() ->
    Table = generate_attack_table(),
    {Type, _} = roll(Table, 1),
    Type.

roll(Table, _) ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    Seed = {A, B, C},

    {Rand, NewSeed} = rand:uniform_s(Seed),
    Value = round(Rand * get_size()),
    io:format("roll: ~p~n", [Value]),
    Offset = Value * 8,
    <<_:Offset, Type ?B, _/binary>> = Table,
    {Type, NewSeed}.
