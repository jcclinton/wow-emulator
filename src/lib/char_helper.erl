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

-module(char_helper).
-compile(export_all).

-include("include/database_records.hrl").
-include("include/data_types.hrl").

-spec gender(gender()) -> non_neg_integer().
gender(male)   -> 0;
gender(female) -> 1;
gender(none)   -> 2.

-spec to_gender(non_neg_integer()) -> gender().
to_gender(0) -> male;
to_gender(1) -> female;
to_gender(2) -> none.

-spec race(race()) -> non_neg_integer().
race(human)     -> 1;
race(orc)       -> 2;
race(dwarf)     -> 3;
race(night_elf) -> 4;
race(undead)    -> 5;
race(tauren)    -> 6;
race(gnome)     -> 7;
race(troll)     -> 8.

-spec to_race(non_neg_integer()) -> race().
to_race(1)  -> human;
to_race(2)  -> orc;
to_race(3)  -> dwarf;
to_race(4)  -> night_elf;
to_race(5)  -> undead;
to_race(6)  -> tauren;
to_race(7)  -> gnome;
to_race(8)  -> troll.

-spec class(class()) -> non_neg_integer().
class(warrior)      -> 1;
class(paladin)      -> 2;
class(hunter)       -> 3;
class(rogue)        -> 4;
class(priest)       -> 5;
class(shaman)       -> 7;
class(mage)         -> 8;
class(warlock)      -> 9;
class(druid)        -> 11.

-spec to_class(non_neg_integer()) -> class().
to_class(1)  -> warrior;
to_class(2)  -> paladin;
to_class(3)  -> hunter;
to_class(4)  -> rogue;
to_class(5)  -> priest;
to_class(7)  -> shaman;
to_class(8)  -> mage;
to_class(9)  -> warlock;
to_class(11) -> druid.
