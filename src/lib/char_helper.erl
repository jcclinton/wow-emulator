-module(char_helper).
-compile(export_all).

-include("include/database_records.hrl").

%% @type gender() = male | female | none.
%% @spec gender(gender()) -> int().
gender(male)   -> 0;
gender(female) -> 1;
gender(none)   -> 2.

%% @spec to_gender(int()) -> gender().
to_gender(0) -> male;
to_gender(1) -> female;
to_gender(2) -> none.

%% @type race() = human | orc | dwarf | night_elf | 
%%                undead | tauren | gnome | troll |
%%                blood_elf | draenei.
%% @spec race(race()) -> int().
race(human)     -> 1;
race(orc)       -> 2;
race(dwarf)     -> 3;
race(night_elf) -> 4;
race(undead)    -> 5;
race(tauren)    -> 6;
race(gnome)     -> 7;
race(troll)     -> 8.

%% @spec to_race(int()) -> race().
to_race(1)  -> human;
to_race(2)  -> orc;
to_race(3)  -> dwarf;
to_race(4)  -> night_elf;
to_race(5)  -> undead;
to_race(6)  -> tauren;
to_race(7)  -> gnome;
to_race(8)  -> troll.

%% @type class() = warrior | paladin | hunter | rogue |
%%                 priest | death_knight | shaman |
%%                 mage | warlock | druid.
%% @spec class(class()) -> int().
class(warrior)      -> 1;
class(paladin)      -> 2;
class(hunter)       -> 3;
class(rogue)        -> 4;
class(priest)       -> 5;
class(shaman)       -> 7;
class(mage)         -> 8;
class(warlock)      -> 9;
class(druid)        -> 11.

%% @spec to_class(int()) -> class().
to_class(1)  -> warrior;
to_class(2)  -> paladin;
to_class(3)  -> hunter;
to_class(4)  -> rogue;
to_class(5)  -> priest;
to_class(7)  -> shaman;
to_class(8)  -> mage;
to_class(9)  -> warlock;
to_class(11) -> druid.

%% @type reputation() = hated | hostile | unfriendly |
%%                      neutral | friendly | honored |
%%                      revered | exalted.
%% @spec reputation(reputation()) -> int().
reputation(hated)      -> 0;
reputation(hostile)    -> 1;
reputation(unfriendly) -> 2;
reputation(neutral)    -> 3;
reputation(friendly)   -> 4;
reputation(honored)    -> 5;
reputation(revered)    -> 6;
reputation(exalted)    -> 7.


%% @type money() = copper | silver | gold.
%% @spec money(money()) -> int().
money(copper) -> 1;
money(silver) -> 100 * money(copper);
money(gold)   -> 100 * money(silver).

%% @type stat() = strength | agility | stamina | intellect | spirit.
%% @spec stat(stat()) -> int().
stat(strength)  -> 0;
stat(agility)   -> 1;
stat(stamina)   -> 2;
stat(intellect) -> 3;
stat(spirit)    -> 4.

%% @type power() = mana | rage | focus | energy |
%%                 happiness | rune | runic_power | health.
%% @spec power(power()) -> int().
power(mana)        -> 0;
power(rage)        -> 1;
power(focus)       -> 2;
power(energy)      -> 3;
power(happiness)   -> 4;
power(health)      -> 16#FFFFFFFE.
