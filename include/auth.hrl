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


-define(wow_success, 16#00).
-define(wow_fail_unknown0, 16#01).
-define(wow_fail_unknown1, 16#02).
-define(wow_fail_banned, 16#03).
-define(wow_fail_unknown_account, 16#04).
-define(wow_fail_incorrect_password, 16#05).
-define(wow_fail_already_online, 16#06).
-define(wow_fail_no_time, 16#07).
-define(wow_fail_db_busy, 16#08).
-define(wow_fail_version_invalid, 16#09).
-define(wow_fail_version_update, 16#0A).
-define(wow_fail_invalid_server, 16#0B).
-define(wow_fail_suspended, 16#0C).
-define(wow_fail_fail_noaccess, 16#0D).
-define(wow_success_survey, 16#0E).
-define(wow_fail_parentcontrol, 16#0F).
-define(wow_fail_locked_enforced, 16#10).
-define(wow_fail_trial_ended, 16#11).
-define(wow_fail_use_battlenet, 16#12).



-define(cmd_auth_logon_challenge, 16#00).
-define(cmd_auth_logon_proof, 16#01).
-define(cmd_auth_reconnect_challenge, 16#02).
-define(cmd_auth_reconnect_proof, 16#03).
-define(cmd_realm_list, 16#10).
-define(cmd_xfer_initiate, 16#30).
-define(cmd_xfer_data, 16#31).
