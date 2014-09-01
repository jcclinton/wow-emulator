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

% network defines
-define(SIL, /signed-little-integer).
-define(IL, /unsigned-little-integer).
-define(IB, /unsigned-big-integer).
-define(f, :32/float-little).

-define(K, :1024?IL).
-define(KB, :1024?IB).
-define(KH, :512?IL).
-define(KHB, :512?IB).

-define(QQ, :256?IL).
-define(QQB, :256?IB).
-define(QH, :128?IL).

-define(SD, :320?IL). % sha1 double
-define(SDB, :320?IB). % sha1 double big endian
-define(SH, :160?IL). % sha1
-define(SHB, :160?IB). % sha1 big endian

-define(Q, :64?IL). % quad
-define(QB, :64?IB). % quad
-define(L, :32?IL). % long
-define(SL, :32?SIL). % signed long
-define(LB, :32?IB). % long
-define(G, :24?IL). % used for guid
-define(W, :16?IL). % word
-define(WO, :16?IB). % word, big endian
-define(B, :8). %byte
