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

-define(updatetype_values, 0).
-define(updatetype_movement, 1).
-define(updatetype_create_object, 2).
-define(updatetype_create_object2, 3).
-define(updatetype_out_of_range_objects, 4).
-define(updatetype_near_objects, 5).

-define(updateflag_none, 16#0000).
-define(updateflag_self, 16#0001).
-define(updateflag_transport, 16#0002).
-define(updateflag_fullguid, 16#0004).
-define(updateflag_highguid, 16#0008).
-define(updateflag_all, 16#0010).
-define(updateflag_living, 16#0020).
-define(updateflag_has_position, 16#0040).
