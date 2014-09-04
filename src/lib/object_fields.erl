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
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURposE.  See the
%%   GNU General Public License for more details.
%%
%%   You should have received a copy of the GNU General Public License along
%%   with this program; if not, write to the Free Software Foundation, Inc.,
%%   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%
%%   World of Warcraft, and all World of Warcraft or Warcraft art, images,
%%   and lore ande copyrighted by Blizzard Entertainment, Inc.

-module(object_fields).

-export([field_data/1, get_total_count/1]).
-export([type/1, fields/1]).

-include("include/data_types.hrl").


-spec fields(atom()) -> non_neg_integer().
fields(Field) ->
	{Index, _} = field_data(Field),
	Index.

-spec type(atom()) -> value_type().
type(Field) ->
	{_, Type} = field_data(Field),
	Type.
	


-spec get_total_count(atom()) -> non_neg_integer().
get_total_count(player) ->
	fields(player_end);
get_total_count(object) ->
	fields(object_end);
get_total_count(item) ->
	fields(item_end);
get_total_count(container) ->
	fields(container_end);
get_total_count(unit) ->
	fields(unit_end);
get_total_count(game_object) ->
	fields(gameobject_end);
get_total_count(dynamic_object) ->
	fields(dynamicobject_end);
get_total_count(corpse) ->
	fields(corpse_end).



-spec field_data(atom()) -> {non_neg_integer(), value_type()}.
field_data(object_field_guid) -> {16#00, uint64};
field_data(object_field_type) -> {16#02, uint32};
field_data(object_field_entry) -> {16#03, uint32};
field_data(object_field_scale_x) -> {16#04, float};
field_data(object_field_padding) -> {16#05, uint32};
field_data(object_end) -> {16#06, uint32};

field_data(item_field_owner) -> {fields(object_end) + 16#00, uint64};
field_data(item_field_contained) -> {fields(object_end) + 16#02, uint64};
field_data(item_field_creator) -> {fields(object_end) + 16#04, uint64};
field_data(item_field_giftcreator) -> {fields(object_end) + 16#06, uint64};
field_data(item_field_stack_count) -> {fields(object_end) + 16#08, uint32};
field_data(item_field_duration) -> {fields(object_end) + 16#09, uint32};
field_data(item_field_spell_charges) -> {fields(object_end) + 16#0A, int32};
field_data(item_field_spell_charges_01) -> {fields(object_end) + 16#0B, int32};
field_data(item_field_spell_charges_02) -> {fields(object_end) + 16#0C, int32};
field_data(item_field_spell_charges_03) -> {fields(object_end) + 16#0D, int32};
field_data(item_field_spell_charges_04) -> {fields(object_end) + 16#0E, int32};
field_data(item_field_flags) -> {fields(object_end) + 16#0F, uint32};
field_data(item_field_enchantment) -> {fields(object_end) + 16#10, uint32};
field_data(item_field_property_seed) -> {fields(object_end) + 16#25, uint32};
field_data(item_field_random_properties_id) -> {fields(object_end) + 16#26, int32};
field_data(item_field_item_text_id) -> {fields(object_end) + 16#27, uint32};
field_data(item_field_durability) -> {fields(object_end) + 16#28, uint32};
field_data(item_field_maxdurability) -> {fields(object_end) + 16#29, uint32};
field_data(item_end) -> {fields(object_end) + 16#2A, uint32};

field_data(container_field_num_slots) -> {fields(item_end) + 16#00, uint32};
field_data(container_align_pad) -> {fields(item_end) + 16#01, uint32};
field_data(container_field_slot_1) -> {fields(item_end) + 16#02, uint64};
field_data(container_field_slot_last) -> {fields(item_end) + 16#38, uint32};
field_data(container_end) -> {fields(item_end) + 16#3A, uint32};

field_data(unit_field_charm) -> {16#00 + fields(object_end), uint64};
field_data(unit_field_summon) -> {16#02 + fields(object_end), uint64};
field_data(unit_field_charmedby) -> {16#04 + fields(object_end), uint64};
field_data(unit_field_summonedby) -> {16#06 + fields(object_end), uint64};
field_data(unit_field_createdby) -> {16#08 + fields(object_end), uint64};
field_data(unit_field_target) -> {16#0A + fields(object_end), uint64};
field_data(unit_field_persuaded) -> {16#0C + fields(object_end), uint64};
field_data(unit_field_channel_object) -> {16#0E + fields(object_end), uint64};
field_data(unit_field_health) -> {16#10 + fields(object_end), uint32};
field_data(unit_field_power1) -> {16#11 + fields(object_end), uint32};
field_data(unit_field_power2) -> {16#12 + fields(object_end), uint32};
field_data(unit_field_power3) -> {16#13 + fields(object_end), uint32};
field_data(unit_field_power4) -> {16#14 + fields(object_end), uint32};
field_data(unit_field_power5) -> {16#15 + fields(object_end), uint32};
field_data(unit_field_maxhealth) -> {16#16 + fields(object_end), uint32};
field_data(unit_field_maxpower1) -> {16#17 + fields(object_end), uint32};
field_data(unit_field_maxpower2) -> {16#18 + fields(object_end), uint32};
field_data(unit_field_maxpower3) -> {16#19 + fields(object_end), uint32};
field_data(unit_field_maxpower4) -> {16#1A + fields(object_end), uint32};
field_data(unit_field_maxpower5) -> {16#1B + fields(object_end), uint32};
field_data(unit_field_level) -> {16#1C + fields(object_end), uint32};
field_data(unit_field_factiontemplate) -> {16#1D + fields(object_end), uint32};
field_data(unit_field_bytes_0) -> {16#1E + fields(object_end), uint8};
field_data(unit_virtual_item_slot_display) -> {16#1F + fields(object_end), uint32};
field_data(unit_virtual_item_slot_display_01) -> {16#20 + fields(object_end), uint32};
field_data(unit_virtual_item_slot_display_02) -> {16#21 + fields(object_end), uint32};
field_data(unit_virtual_item_info) -> {16#22 + fields(object_end), uint32};
field_data(unit_virtual_item_info_01) -> {16#23 + fields(object_end), uint32};
field_data(unit_virtual_item_info_02) -> {16#24 + fields(object_end), uint32};
field_data(unit_virtual_item_info_03) -> {16#25 + fields(object_end), uint32};
field_data(unit_virtual_item_info_04) -> {16#26 + fields(object_end), uint32};
field_data(unit_virtual_item_info_05) -> {16#27 + fields(object_end), uint32};
field_data(unit_field_flags) -> {16#28 + fields(object_end), uint32};
field_data(unit_field_aura) -> {16#29 + fields(object_end), uint32};
field_data(unit_field_aura_last) -> {16#58 + fields(object_end), uint32};
field_data(unit_field_auraflags) -> {16#59 + fields(object_end), uint32};
field_data(unit_field_auraflags_01) -> {16#5a + fields(object_end), uint32};
field_data(unit_field_auraflags_02) -> {16#5b + fields(object_end), uint32};
field_data(unit_field_auraflags_03) -> {16#5c + fields(object_end), uint32};
field_data(unit_field_auraflags_04) -> {16#5d + fields(object_end), uint32};
field_data(unit_field_auraflags_05) -> {16#5e + fields(object_end), uint32};
field_data(unit_field_auralevels) -> {16#5f + fields(object_end), uint32};
field_data(unit_field_auralevels_last) -> {16#6a + fields(object_end), uint32};
field_data(unit_field_auraapplications) -> {16#6b + fields(object_end), uint32};
field_data(unit_field_auraapplications_last) -> {16#76 + fields(object_end), uint32};
field_data(unit_field_aurastate) -> {16#77 + fields(object_end), uint32};
field_data(unit_field_baseattacktime) -> {16#78 + fields(object_end), float};
field_data(unit_field_offhandattacktime) -> {16#79 + fields(object_end), float};
field_data(unit_field_rangedattacktime) -> {16#7a + fields(object_end), float};
field_data(unit_field_boundingradius) -> {16#7b + fields(object_end), float};
field_data(unit_field_combatreach) -> {16#7c + fields(object_end), float};
field_data(unit_field_displayid) -> {16#7d + fields(object_end), uint32};
field_data(unit_field_nativedisplayid) -> {16#7e + fields(object_end), uint32};
field_data(unit_field_mountdisplayid) -> {16#7f + fields(object_end), uint32};
field_data(unit_field_mindamage) -> {16#80 + fields(object_end), float};
field_data(unit_field_maxdamage) -> {16#81 + fields(object_end), float};
field_data(unit_field_minoffhanddamage) -> {16#82 + fields(object_end), float};
field_data(unit_field_maxoffhanddamage) -> {16#83 + fields(object_end), float};
field_data(unit_field_bytes_1) -> {16#84 + fields(object_end), uint8};
field_data(unit_field_petnumber) -> {16#85 + fields(object_end), uint32};
field_data(unit_field_pet_name_timestamp) -> {16#86 + fields(object_end), uint32};
field_data(unit_field_petexperience) -> {16#87 + fields(object_end), uint32};
field_data(unit_field_petnextlevelexp) -> {16#88 + fields(object_end), uint32};
field_data(unit_dynamic_flags) -> {16#89 + fields(object_end), uint32};
field_data(unit_channel_spell) -> {16#8a + fields(object_end), uint32};
field_data(unit_mod_cast_speed) -> {16#8b + fields(object_end), uint32};
field_data(unit_created_by_spell) -> {16#8c + fields(object_end), uint32};
field_data(unit_npc_flags) -> {16#8d + fields(object_end), uint32};
field_data(unit_npc_emotestate) -> {16#8e + fields(object_end), uint32};
field_data(unit_training_points) -> {16#8f + fields(object_end), uint32};
field_data(unit_field_stat0) -> {16#90 + fields(object_end), float};
field_data(unit_field_stat1) -> {16#91 + fields(object_end), float};
field_data(unit_field_stat2) -> {16#92 + fields(object_end), float};
field_data(unit_field_stat3) -> {16#93 + fields(object_end), float};
field_data(unit_field_stat4) -> {16#94 + fields(object_end), float};
field_data(unit_field_resistances) -> {16#95 + fields(object_end), uint32};
field_data(unit_field_resistances_01) -> {16#96 + fields(object_end), uint32};
field_data(unit_field_resistances_02) -> {16#97 + fields(object_end), uint32};
field_data(unit_field_resistances_03) -> {16#98 + fields(object_end), uint32};
field_data(unit_field_resistances_04) -> {16#99 + fields(object_end), uint32};
field_data(unit_field_resistances_05) -> {16#9a + fields(object_end), uint32};
field_data(unit_field_resistances_06) -> {16#9b + fields(object_end), uint32};
field_data(unit_field_base_mana) -> {16#9c + fields(object_end), uint32};
field_data(unit_field_base_health) -> {16#9d + fields(object_end), uint32};
field_data(unit_field_bytes_2) -> {16#9e + fields(object_end), uint8};
field_data(unit_field_attack_power) -> {16#9f + fields(object_end), int32};
field_data(unit_field_attack_power_mods) -> {16#a0 + fields(object_end), int32};
field_data(unit_field_attack_power_multiplier) -> {16#a1 + fields(object_end), float};
field_data(unit_field_ranged_attack_power) -> {16#a2 + fields(object_end), int32};
field_data(unit_field_ranged_attack_power_mods) -> {16#a3 + fields(object_end), int32};
field_data(unit_field_ranged_attack_power_multiplier) -> {16#a4 + fields(object_end), float};
field_data(unit_field_minrangeddamage) -> {16#a5 + fields(object_end), float};
field_data(unit_field_maxrangeddamage) -> {16#a6 + fields(object_end), float};
field_data(unit_field_power_cost_modifier) -> {16#a7 + fields(object_end), int32};
field_data(unit_field_power_cost_modifier_01) -> {16#a8 + fields(object_end), int32};
field_data(unit_field_power_cost_modifier_02) -> {16#a9 + fields(object_end), int32};
field_data(unit_field_power_cost_modifier_03) -> {16#aa + fields(object_end), int32};
field_data(unit_field_power_cost_modifier_04) -> {16#ab + fields(object_end), int32};
field_data(unit_field_power_cost_modifier_05) -> {16#ac + fields(object_end), int32};
field_data(unit_field_power_cost_modifier_06) -> {16#ad + fields(object_end), int32};
field_data(unit_field_power_cost_multiplier) -> {16#ae + fields(object_end), float};
field_data(unit_field_power_cost_multiplier_01) -> {16#af + fields(object_end), float};
field_data(unit_field_power_cost_multiplier_02) -> {16#b0 + fields(object_end), float};
field_data(unit_field_power_cost_multiplier_03) -> {16#b1 + fields(object_end), float};
field_data(unit_field_power_cost_multiplier_04) -> {16#b2 + fields(object_end), float};
field_data(unit_field_power_cost_multiplier_05) -> {16#b3 + fields(object_end), float};
field_data(unit_field_power_cost_multiplier_06) -> {16#b4 + fields(object_end), float};
field_data(unit_field_padding) -> {16#b5 + fields(object_end), uint32};
field_data(unit_end) -> {16#b6 + fields(object_end), uint32};

field_data(player_duel_arbiter) -> {16#00 + fields(unit_end), uint64};
field_data(player_flags) -> {16#02 + fields(unit_end), uint32};
field_data(player_guildid) -> {16#03 + fields(unit_end), uint32};
field_data(player_guildrank) -> {16#04 + fields(unit_end), uint32};
field_data(player_bytes) -> {16#05 + fields(unit_end), uint8};
field_data(player_bytes_2) -> {16#06 + fields(unit_end), uint8};
field_data(player_bytes_3) -> {16#07 + fields(unit_end), uint8};
field_data(player_duel_team) -> {16#08 + fields(unit_end), uint32};
field_data(player_guild_timestamp) -> {16#09 + fields(unit_end), uint32};
field_data(player_quest_log_1_1) -> {16#0A + fields(unit_end), uint32};
field_data(player_quest_log_1_2) -> {16#0B + fields(unit_end), uint32};
field_data(player_quest_log_1_3) -> {16#0C + fields(unit_end), uint32};
field_data(player_quest_log_last_1) -> {16#43 + fields(unit_end), uint32};
field_data(player_quest_log_last_2) -> {16#44 + fields(unit_end), uint32};
field_data(player_quest_log_last_3) -> {16#45 + fields(unit_end), uint32};
field_data(player_visible_item_1_creator) -> {16#46 + fields(unit_end), uint64};
field_data(player_visible_item_1_0) -> {16#48 + fields(unit_end), uint32};
field_data(player_visible_item_1_properties) -> {16#50 + fields(unit_end), uint32};
field_data(player_visible_item_1_pad) -> {16#51 + fields(unit_end), uint32};
field_data(player_visible_item_last_creator) -> {16#11e + fields(unit_end), uint32};
field_data(player_visible_item_last_0) -> {16#120 + fields(unit_end), uint32};
field_data(player_visible_item_last_properties) -> {16#128 + fields(unit_end), uint32};
field_data(player_visible_item_last_pad) -> {16#129 + fields(unit_end), uint32};
field_data(player_field_inv_slot_head) -> {16#12a + fields(unit_end), uint64};
field_data(player_field_pack_slot_1) -> {16#158 + fields(unit_end), uint64};
field_data(player_field_pack_slot_last) -> {16#176 + fields(unit_end), uint32};
field_data(player_field_bank_slot_1) -> {16#178 + fields(unit_end), uint64};
field_data(player_field_bank_slot_last) -> {16#1a6 + fields(unit_end), uint32};
field_data(player_field_bankbag_slot_1) -> {16#1a8 + fields(unit_end), uint64};
field_data(player_field_bankbag_slot_last) -> {16#ab2 + fields(unit_end), uint32};
field_data(player_field_vendorbuyback_slot_1) -> {16#1b4 + fields(unit_end), uint64};
field_data(player_field_vendorbuyback_slot_last) -> {16#1ca + fields(unit_end), uint32};
field_data(player_field_keyring_slot_1) -> {16#1cc + fields(unit_end), uint64};
field_data(player_field_keyring_slot_last) -> {16#20a + fields(unit_end), uint32};
field_data(player_farsight) -> {16#20c + fields(unit_end), uint64};
field_data(player_field_combo_target) -> {16#20e + fields(unit_end), uint64};
field_data(player_xp) -> {16#210 + fields(unit_end), uint32};
field_data(player_next_level_xp) -> {16#211 + fields(unit_end), uint32};
field_data(player_skill_info_1_1) -> {16#212 + fields(unit_end), uint32};
field_data(player_character_points1) -> {16#392 + fields(unit_end), uint32};
field_data(player_character_points2) -> {16#393 + fields(unit_end), uint32};
field_data(player_track_creatures) -> {16#394 + fields(unit_end), uint32};
field_data(player_track_resources) -> {16#395 + fields(unit_end), uint32};
field_data(player_block_percentage) -> {16#396 + fields(unit_end), float};
field_data(player_dodge_percentage) -> {16#397 + fields(unit_end), float};
field_data(player_parry_percentage) -> {16#398 + fields(unit_end), float};
field_data(player_crit_percentage) -> {16#399 + fields(unit_end), float};
field_data(player_ranged_crit_percentage) -> {16#39a + fields(unit_end), float};
field_data(player_explored_zones_1) -> {16#39b + fields(unit_end), uint32};
field_data(player_rest_state_experience) -> {16#3db + fields(unit_end), uint32};
field_data(player_field_coinage) -> {16#3dc + fields(unit_end), uint32};
field_data(player_field_posstat0) -> {16#3DD + fields(unit_end), float};
field_data(player_field_posstat1) -> {16#3DE + fields(unit_end), float};
field_data(player_field_posstat2) -> {16#3DF + fields(unit_end), float};
field_data(player_field_posstat3) -> {16#3E0 + fields(unit_end), float};
field_data(player_field_posstat4) -> {16#3E1 + fields(unit_end), float};
field_data(player_field_negstat0) -> {16#3E2 + fields(unit_end), float};
field_data(player_field_negstat1) -> {16#3E3 + fields(unit_end), float};
field_data(player_field_negstat2) -> {16#3E4 + fields(unit_end), float};
field_data(player_field_negstat3) -> {16#3E5 + fields(unit_end), float};
field_data(player_field_negstat4) -> {16#3E6 + fields(unit_end), float};
field_data(player_field_resistancebuffmodspositive) -> {16#3E7 + fields(unit_end), float};
field_data(player_field_resistancebuffmodsnegative) -> {16#3EE + fields(unit_end), float};
field_data(player_field_mod_damage_done_pos) -> {16#3F5 + fields(unit_end), uint32};
field_data(player_field_mod_damage_done_neg) -> {16#3FC + fields(unit_end), uint32};
field_data(player_field_mod_damage_done_pct) -> {16#403 + fields(unit_end), float};
field_data(player_field_bytes) -> {16#40A + fields(unit_end), uint8};
field_data(player_ammo_id) -> {16#40B + fields(unit_end), uint32};
field_data(player_self_res_spell) -> {16#40C + fields(unit_end), uint32};
field_data(player_field_pvp_medals) -> {16#40D + fields(unit_end), uint32};
field_data(player_field_buyback_price_1) -> {16#40E + fields(unit_end), uint32};
field_data(player_field_buyback_price_last) -> {16#419 + fields(unit_end), uint32};
field_data(player_field_buyback_timestamp_1) -> {16#41A + fields(unit_end), uint32};
field_data(player_field_buyback_timestamp_last) -> {16#425 + fields(unit_end), uint32};
field_data(player_field_session_kills) -> {16#426 + fields(unit_end), uint32};
field_data(player_field_yesterday_kills) -> {16#427 + fields(unit_end), uint32};
field_data(player_field_last_week_kills) -> {16#428 + fields(unit_end), uint32};
field_data(player_field_this_week_kills) -> {16#429 + fields(unit_end), uint32};
field_data(player_field_this_week_contribution) -> {16#42a + fields(unit_end), uint32};
field_data(player_field_lifetime_honorable_kills) -> {16#42b + fields(unit_end), uint32};
field_data(player_field_lifetime_dishonorable_kills) -> {16#42c + fields(unit_end), uint32};
field_data(player_field_yesterday_contribution) -> {16#42d + fields(unit_end), uint32};
field_data(player_field_last_week_contribution) -> {16#42e + fields(unit_end), uint32};
field_data(player_field_last_week_rank) -> {16#42f + fields(unit_end), uint32};
field_data(player_field_bytes2) -> {16#430 + fields(unit_end), uint8};
field_data(player_field_watched_faction_index) -> {16#431 + fields(unit_end), int32};
field_data(player_field_combat_rating_1) -> {16#432 + fields(unit_end), uint32};
field_data(player_end) -> {16#446 + fields(unit_end), uint32};

field_data(object_field_created_by) -> {fields(object_end) + 16#00, uint64};
field_data(gameobject_displayid) -> {fields(object_end) + 16#02, uint32};
field_data(gameobject_flags) -> {fields(object_end) + 16#03, uint32};
field_data(gameobject_rotation) -> {fields(object_end) + 16#04, float};
field_data(gameobject_state) -> {fields(object_end) + 16#08, uint32};
field_data(gameobject_pos_x) -> {fields(object_end) + 16#09, float};
field_data(gameobject_pos_y) -> {fields(object_end) + 16#0A, float};
field_data(gameobject_pos_z) -> {fields(object_end) + 16#0B, float};
field_data(gameobject_facing) -> {fields(object_end) + 16#0C, float};
field_data(gameobject_dyn_flags) -> {fields(object_end) + 16#0D, uint32};
field_data(gameobject_faction) -> {fields(object_end) + 16#0E, uint32};
field_data(gameobject_type_id) -> {fields(object_end) + 16#0F, uint32};
field_data(gameobject_level) -> {fields(object_end) + 16#10, uint32};
field_data(gameobject_artkit) -> {fields(object_end) + 16#11, uint32};
field_data(gameobject_animprogress) -> {fields(object_end) + 16#12, uint32};
field_data(gameobject_padding) -> {fields(object_end) + 16#13, uint32};
field_data(gameobject_end) -> {fields(object_end) + 16#14, uint32};

field_data(dynamicobject_caster) -> {fields(object_end) + 16#00, uint64};
field_data(dynamicobject_bytes) -> {fields(object_end) + 16#02, uint8};
field_data(dynamicobject_spellid) -> {fields(object_end) + 16#03, uint32};
field_data(dynamicobject_radius) -> {fields(object_end) + 16#04, float};
field_data(dynamicobject_pos_x) -> {fields(object_end) + 16#05, float};
field_data(dynamicobject_pos_y) -> {fields(object_end) + 16#06, float};
field_data(dynamicobject_pos_z) -> {fields(object_end) + 16#07, float};
field_data(dynamicobject_facing) -> {fields(object_end) + 16#08, float};
field_data(dynamicobject_pad) -> {fields(object_end) + 16#09, uint32};
field_data(dynamicobject_end) -> {fields(object_end) + 16#0A, uint32};

field_data(corpse_fields_owner) -> {fields(object_end) + 16#00, uint64};
field_data(corpse_fields_facing) -> {fields(object_end) + 16#02, float};
field_data(corpse_fields_pos_x) -> {fields(object_end) + 16#03, float};
field_data(corpse_fields_pos_y) -> {fields(object_end) + 16#04, float};
field_data(corpse_fields_pos_z) -> {fields(object_end) + 16#05, float};
field_data(corpse_fields_display_id) -> {fields(object_end) + 16#06, uint32};
field_data(corpse_fields_item) -> {fields(object_end) + 16#07, uint32};
field_data(corpse_fields_bytes_1) -> {fields(object_end) + 16#1A, uint8};
field_data(corpse_fields_bytes_2) -> {fields(object_end) + 16#1B, uint8};
field_data(corpse_fields_guild) -> {fields(object_end) + 16#1C, uint32};
field_data(corpse_fields_flags) -> {fields(object_end) + 16#1D, uint32};
field_data(corpse_fields_dynamic_flags) -> {fields(object_end) + 16#1E, uint32};
field_data(corpse_fields_pad) -> {fields(object_end) + 16#1F, uint32};
field_data(corpse_end) -> {fields(object_end) + 16#20, uint32}.
