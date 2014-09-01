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


-define(moveflag_move_stop, 16#00000000).
-define(moveflag_move_forward, 16#00000001).
-define(moveflag_move_backward, 16#00000002).
-define(moveflag_strafe_left, 16#00000004).
-define(moveflag_strafe_right, 16#00000008).
-define(moveflag_turn_left, 16#00000010).
-define(moveflag_turn_right, 16#00000020).
-define(moveflag_pitch_up, 16#00000040).
-define(moveflag_pitch_down, 16#00000080).

-define(moveflag_walk_mode, 16#00000100).
-define(moveflag_taxi, 16#02000000).

-define(moveflag_no_collision, 16#00000400).
-define(moveflag_flying, 16#00000800).
-define(moveflag_redirected, 16#00001000).
-define(moveflag_falling, 16#00002000).
-define(moveflag_falling_far, 16#00004000).
-define(moveflag_free_falling, 16#00008000).

-define(moveflag_tb_pending_stop, 16#00010000).
-define(moveflag_tb_pending_unstrafe, 16#00020000).
-define(moveflag_tb_pending_fall, 16#00040000).
-define(moveflag_tb_pending_forward, 16#00080000).
-define(moveflag_tb_pending_backward, 16#00100000).
-define(moveflag_swimming, 16#00200000).
-define(moveflag_flying_pitch_up, 16#00400000),
-define(moveflag_tb_moved, 16#00800000).

-define(moveflag_air_suspension, 16#01000000).
-define(moveflag_spline_mover, 16#04000000).
-define(moveflag_immobilized, 16#08000000).
-define(moveflag_water_walk, 16#10000000).
-define(moveflag_feather_fall, 16#20000000).
-define(moveflag_levitate, 16#40000000).
-define(moveflag_local, 16#80000000).

-define(moveflag_moving_mask, 16#03).
-define(moveflag_strafing_mask, 16#0C).
-define(moveflag_turning_mask, 16#30).
-define(moveflag_falling_mask, 16#6000).
-define(moveflag_motion_mask, 16#E00F).
-define(moveflag_pending_mask, 16#7F0000).
-define(moveflag_pending_strafe_mask, 16#600000).
-define(moveflag_pending_move_mask, 16#180000).
-define(moveflag_full_falling_mask, 16#E000).
