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


-define(typemask_object, 16#0001).
-define(typemask_item, 16#0002).
-define(typemask_container, 16#0004).
-define(typemask_unit, 16#0008).
-define(typemask_player, 16#0010).
-define(typemask_gameobject, 16#0020).
-define(typemask_dynamicobject, 16#0040).
-define(typemask_corpse, 16#0080).


-define(typeid_object, 0).
-define(typeid_item, 1).
-define(typeid_container, 2).
-define(typeid_unit, 3).
-define(typeid_player, 4).
-define(typeid_gameobject, 5).
-define(typeid_dynamicobject, 6).
-define(typeid_corpse, 7).


-define(highguid_item, 16#4000).
-define(highguid_container, 16#4000).
-define(highguid_player, 16#0000).
-define(highguid_gameobject, 16#F110).
-define(highguid_transport, 16#F120).
-define(highguid_unit, 16#F130).
-define(highguid_pet, 16#F140).
-define(highguid_dynamicobject, 16#F100).
-define(highguid_corpse, 16#F101).
-define(highguid_mo_transport, 16#1FC0).
