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


-define(cast_flag_none, 16#00000000).
-define(cast_flag_hidden_combatlog, 16#00000001).
-define(cast_flag_unknown2, 16#00000002).
-define(cast_flag_unknown3, 16#00000004).
-define(cast_flag_unknown4, 16#00000008).
-define(cast_flag_unknown5, 16#00000010).
-define(cast_flag_ammo, 16#00000020).
-define(cast_flag_unknown7, 16#00000040).
-define(cast_flag_unknown8, 16#00000080).
-define(cast_flag_unknown9, 16#00000100).


-define(spell_failed_affecting_combat, 16#00).
-define(spell_failed_already_at_full_health, 16#01).
-define(spell_failed_already_at_full_mana, 16#02).
-define(spell_failed_already_being_tamed, 16#03).
-define(spell_failed_already_have_charm, 16#04).
-define(spell_failed_already_have_summon, 16#05).
-define(spell_failed_already_open, 16#06).
-define(spell_failed_more_powerful_spell_active, 16#07).
%-define(spell_failed_autotrack_interrupted, 16#08).
-define(spell_failed_bad_implicit_targets, 16#09).
-define(spell_failed_bad_targets, 16#0A).
-define(spell_failed_cant_be_charmed, 16#0B).
-define(spell_failed_cant_be_disenchanted, 16#0C).
-define(spell_failed_cant_be_prospected, 16#0D).
-define(spell_failed_cant_cast_on_tapped, 16#0E).
-define(spell_failed_cant_duel_while_invisible, 16#0F).
-define(spell_failed_cant_duel_while_stealthed, 16#10).
-define(spell_failed_cant_too_close_to_enemy, 16#11).
-define(spell_failed_cant_do_that_yet, 16#12).
-define(spell_failed_caster_dead, 16#13).
-define(spell_failed_charmed, 16#14).
-define(spell_failed_chest_in_use, 16#15).
-define(spell_failed_confused, 16#16).
-define(spell_failed_dont_report, 16#17).
-define(spell_failed_equipped_item, 16#18).
-define(spell_failed_equipped_item_class, 16#19).
-define(spell_failed_equipped_item_class_mainhand, 16#1A).
-define(spell_failed_equipped_item_class_offhand, 16#1B).
-define(spell_failed_error, 16#1C).
-define(spell_failed_fizzle, 16#1D).
-define(spell_failed_fleeing, 16#1E).
-define(spell_failed_food_lowlevel, 16#1F).
-define(spell_failed_highlevel, 16#20).
%-define(spell_failed_hunger_satiated, 16#21).
-define(spell_failed_immune, 16#22).
-define(spell_failed_interrupted, 16#23).
-define(spell_failed_interrupted_combat, 16#24).
-define(spell_failed_item_already_enchanted, 16#25).
-define(spell_failed_item_gone, 16#26).
-define(spell_failed_enchant_not_existing_item, 16#27).
-define(spell_failed_item_not_ready, 16#28).
-define(spell_failed_level_requirement, 16#29).
-define(spell_failed_line_of_sight, 16#2A).
-define(spell_failed_lowlevel, 16#2B).
-define(spell_failed_skill_not_high_enough, 16#2C).
-define(spell_failed_mainhand_empty, 16#2D).
-define(spell_failed_moving, 16#2E).
-define(spell_failed_need_ammo, 16#2F).
-define(spell_failed_need_requires_something, 16#30).
-define(spell_failed_need_exotic_ammo, 16#31).
-define(spell_failed_nopath, 16#32).
-define(spell_failed_not_behind, 16#33).
-define(spell_failed_not_fishable, 16#34).
-define(spell_failed_not_here, 16#35).
-define(spell_failed_not_infront, 16#36).
-define(spell_failed_not_in_control, 16#37).
-define(spell_failed_not_known, 16#38).
-define(spell_failed_not_mounted, 16#39).
-define(spell_failed_not_on_taxi, 16#3A).
-define(spell_failed_not_on_transport, 16#3B).
-define(spell_failed_not_ready, 16#3C).
-define(spell_failed_not_shapeshift, 16#3D).
-define(spell_failed_not_standing, 16#3E).
-define(spell_failed_not_tradeable, 16#3F).
-define(spell_failed_not_trading, 16#40).
-define(spell_failed_not_unsheathed, 16#41).
-define(spell_failed_not_while_ghost, 16#42).
-define(spell_failed_no_ammo, 16#43).
-define(spell_failed_no_charges_remain, 16#44).
-define(spell_failed_no_champion, 16#45).
-define(spell_failed_no_combo_points, 16#46).
-define(spell_failed_no_dueling, 16#47).
-define(spell_failed_no_endurance, 16#48).
-define(spell_failed_no_fish, 16#49).
-define(spell_failed_no_items_while_shapeshifted, 16#4A).
-define(spell_failed_no_mounts_allowed, 16#4B).
-define(spell_failed_no_pet, 16#4C).
-define(spell_failed_no_power, 16#4D).
-define(spell_failed_nothing_to_dispel, 16#4E).
-define(spell_failed_nothing_to_steal, 16#4F).
-define(spell_failed_only_abovewater, 16#50).
-define(spell_failed_only_daytime, 16#51).
-define(spell_failed_only_indoors, 16#52).
-define(spell_failed_only_mounted, 16#53).
-define(spell_failed_only_nighttime, 16#54).
-define(spell_failed_only_outdoors, 16#55).
-define(spell_failed_only_shapeshift, 16#56).
-define(spell_failed_only_stealthed, 16#57).
-define(spell_failed_only_underwater, 16#58).
-define(spell_failed_out_of_range, 16#59).
-define(spell_failed_pacified, 16#5A).
-define(spell_failed_possessed, 16#5B).
%-define(spell_failed_reagents, 16#5C).
-define(spell_failed_requires_area, 16#5D).
-define(spell_failed_requires_spell_focus, 16#5E).
-define(spell_failed_rooted, 16#5F).
-define(spell_failed_silenced, 16#60).
-define(spell_failed_spell_in_progress, 16#61).
-define(spell_failed_spell_learned, 16#62).
-define(spell_failed_spell_unavailable, 16#63).
-define(spell_failed_stunned, 16#64).
-define(spell_failed_targets_dead, 16#65).
-define(spell_failed_target_affecting_combat, 16#66).
-define(spell_failed_target_aurastate, 16#67).
-define(spell_failed_target_dueling, 16#68).
-define(spell_failed_target_enemy, 16#69).
-define(spell_failed_target_enraged, 16#6A).
-define(spell_failed_target_friendly, 16#6B).
-define(spell_failed_target_in_combat, 16#6C).
-define(spell_failed_target_is_player, 16#6D).
-define(spell_failed_target_not_dead, 16#6E).
-define(spell_failed_target_not_in_party, 16#6F).
-define(spell_failed_target_not_looted, 16#70).
-define(spell_failed_target_not_player, 16#71).
-define(spell_failed_target_no_pockets, 16#72).
-define(spell_failed_target_no_weapons, 16#73).
-define(spell_failed_target_unskinnable, 16#74).
-define(spell_failed_thirst_satiated, 16#75).
-define(spell_failed_too_close, 16#76).
-define(spell_failed_too_many_of_item, 16#77).
%-define(spell_failed_totems, 16#78).
-define(spell_failed_training_points, 16#79).
-define(spell_failed_try_again, 16#7A).
-define(spell_failed_unit_not_behind, 16#7B).
-define(spell_failed_unit_not_infront, 16#7C).
-define(spell_failed_wrong_pet_food, 16#7D).
-define(spell_failed_not_while_fatigued, 16#7E).
-define(spell_failed_target_not_in_instance, 16#7F).
-define(spell_failed_not_while_trading, 16#80).
-define(spell_failed_target_not_in_raid, 16#81).
-define(spell_failed_disenchant_while_looting, 16#82).
-define(spell_failed_prospect_while_looting, 16#83).
%-define(spell_failed_prospect_need_more, 16#85).
-define(spell_failed_target_freeforall, 16#85).
-define(spell_failed_no_edible_corpses, 16#86).
-define(spell_failed_only_battlegrounds, 16#87).
-define(spell_failed_target_not_ghost, 16#88).
-define(spell_failed_too_many_skills, 16#89).
-define(spell_failed_cant_use_new_item, 16#8A).
-define(spell_failed_wrong_weather, 16#8B).
-define(spell_failed_damage_immune, 16#8C).
-define(spell_failed_prevented_by_mechanic, 16#8D).
-define(spell_failed_play_time, 16#8E).
-define(spell_failed_reputation, 16#8F).
-define(spell_failed_min_skill, 16#90).
-define(spell_failed_unknown, 16#91).
-define(spell_cast_ok, 16#FF).




-define(target_flag_self, 16#00000000).
-define(target_flag_unused1, 16#00000001).
-define(target_flag_unit, 16#00000002).
-define(target_flag_unused2, 16#00000004).
-define(target_flag_unused3, 16#00000008).
-define(target_flag_item, 16#00000010).
-define(target_flag_source_location, 16#00000020).
-define(target_flag_dest_location, 16#00000040).
-define(target_flag_object_unk, 16#00000080).
-define(target_flag_unit_unk, 16#00000100).
-define(target_flag_pvp_corpse, 16#00000200).
-define(target_flag_unit_corpse, 16#00000400).
-define(target_flag_object, 16#00000800).
-define(target_flag_trade_item, 16#00001000).
-define(target_flag_string, 16#00002000).
-define(target_flag_unk1, 16#00004000).
-define(target_flag_corpse, 16#00008000).
-define(target_flag_unk2, 16#00010000).





-define(spellfamily_generic, 0).
-define(spellfamily_environment, 1).
-define(spellfamily_mage, 3).
-define(spellfamily_warrior, 4).
-define(spellfamily_warlock, 5).
-define(spellfamily_priest, 6).
-define(spellfamily_druid, 7).
-define(spellfamily_rogue, 8).
-define(spellfamily_hunter, 9).
-define(spellfamily_paladin, 10).
-define(spellfamily_shaman, 11).
-define(spellfamily_potion, 13).












-define(spell_aura_none, 0).
-define(spell_aura_bind_sight, 1).
-define(spell_aura_mod_possess, 2).
-define(spell_aura_periodic_damage, 3).
-define(spell_aura_dummy, 4).
-define(spell_aura_mod_confuse, 5).
-define(spell_aura_mod_charm, 6).
-define(spell_aura_mod_fear, 7).
-define(spell_aura_periodic_heal, 8).
-define(spell_aura_mod_attackspeed, 9).
-define(spell_aura_mod_threat, 10).
-define(spell_aura_mod_taunt, 11).
-define(spell_aura_mod_stun, 12).
-define(spell_aura_mod_damage_done, 13).
-define(spell_aura_mod_damage_taken, 14).
-define(spell_aura_damage_shield, 15).
-define(spell_aura_mod_stealth, 16).
-define(spell_aura_mod_stealth_detect, 17).
-define(spell_aura_mod_invisibility, 18).
-define(spell_aura_mod_invisibility_detection, 19).
-define(spell_aura_obs_mod_health, 20).
-define(spell_aura_obs_mod_mana, 21).
-define(spell_aura_mod_resistance, 22).
-define(spell_aura_periodic_trigger_spell, 23).
-define(spell_aura_periodic_energize, 24).
-define(spell_aura_mod_pacify, 25).
-define(spell_aura_mod_root, 26).
-define(spell_aura_mod_silence, 27).
-define(spell_aura_reflect_spells, 28).
-define(spell_aura_mod_stat, 29).
-define(spell_aura_mod_skill, 30).
-define(spell_aura_mod_increase_speed, 31).
-define(spell_aura_mod_increase_mounted_speed, 32).
-define(spell_aura_mod_decrease_speed, 33).
-define(spell_aura_mod_increase_health, 34).
-define(spell_aura_mod_increase_energy, 35).
-define(spell_aura_mod_shapeshift, 36).
-define(spell_aura_effect_immunity, 37).
-define(spell_aura_state_immunity, 38).
-define(spell_aura_school_immunity, 39).
-define(spell_aura_damage_immunity, 40).
-define(spell_aura_dispel_immunity, 41).
-define(spell_aura_proc_trigger_spell, 42).
-define(spell_aura_proc_trigger_damage, 43).
-define(spell_aura_track_creatures, 44).
-define(spell_aura_track_resources, 45).
-define(spell_aura_46, 46).
-define(spell_aura_mod_parry_percent, 47).
-define(spell_aura_48, 48).
-define(spell_aura_mod_dodge_percent, 49).
-define(spell_aura_mod_block_skill, 50).
-define(spell_aura_mod_block_percent, 51).
-define(spell_aura_mod_crit_percent, 52).
-define(spell_aura_periodic_leech, 53).
-define(spell_aura_mod_hit_chance, 54).
-define(spell_aura_mod_spell_hit_chance, 55).
-define(spell_aura_transform, 56).
-define(spell_aura_mod_spell_crit_chance, 57).
-define(spell_aura_mod_increase_swim_speed, 58).
-define(spell_aura_mod_damage_done_creature, 59).
-define(spell_aura_mod_pacify_silence, 60).
-define(spell_aura_mod_scale, 61).
-define(spell_aura_periodic_health_funnel, 62).
-define(spell_aura_periodic_mana_funnel, 63).
-define(spell_aura_periodic_mana_leech, 64).
-define(spell_aura_mod_casting_speed_not_stack, 65).
-define(spell_aura_feign_death, 66).
-define(spell_aura_mod_disarm, 67).
-define(spell_aura_mod_stalked, 68).
-define(spell_aura_school_absorb, 69).
-define(spell_aura_extra_attacks, 70).
-define(spell_aura_mod_spell_crit_chance_school, 71).
-define(spell_aura_mod_power_cost_school_pct, 72).
-define(spell_aura_mod_power_cost_school, 73).
-define(spell_aura_reflect_spells_school, 74).
-define(spell_aura_mod_language, 75).
-define(spell_aura_far_sight, 76).
-define(spell_aura_mechanic_immunity, 77).
-define(spell_aura_mounted, 78).
-define(spell_aura_mod_damage_percent_done, 79).
-define(spell_aura_mod_percent_stat, 80).
-define(spell_aura_split_damage_pct, 81).
-define(spell_aura_water_breathing, 82).
-define(spell_aura_mod_base_resistance, 83).
-define(spell_aura_mod_regen, 84).
-define(spell_aura_mod_power_regen, 85).
-define(spell_aura_channel_death_item, 86).
-define(spell_aura_mod_damage_percent_taken, 87).
-define(spell_aura_mod_health_regen_percent, 88).
-define(spell_aura_periodic_damage_percent, 89).
-define(spell_aura_mod_resist_chance, 90).
-define(spell_aura_mod_detect_range, 91).
-define(spell_aura_prevents_fleeing, 92).
-define(spell_aura_mod_unattackable, 93).
-define(spell_aura_interrupt_regen, 94).
-define(spell_aura_ghost, 95).
-define(spell_aura_spell_magnet, 96).
-define(spell_aura_mana_shield, 97).
-define(spell_aura_mod_skill_talent, 98).
-define(spell_aura_mod_attack_power, 99).
-define(spell_aura_auras_visible, 100).
-define(spell_aura_mod_resistance_pct, 101).
-define(spell_aura_mod_melee_attack_power_versus, 102).
-define(spell_aura_mod_total_threat, 103).
-define(spell_aura_water_walk, 104).
-define(spell_aura_feather_fall, 105).
-define(spell_aura_hover, 106).
-define(spell_aura_add_flat_modifier, 107).
-define(spell_aura_add_pct_modifier, 108).
-define(spell_aura_add_target_trigger, 109).
-define(spell_aura_mod_power_regen_percent, 110).
-define(spell_aura_add_caster_hit_trigger, 111).
-define(spell_aura_override_class_scripts, 112).
-define(spell_aura_mod_ranged_damage_taken, 113).
-define(spell_aura_mod_ranged_damage_taken_pct, 114).
-define(spell_aura_mod_healing, 115).
-define(spell_aura_mod_regen_during_combat, 116).
-define(spell_aura_mod_mechanic_resistance, 117).
-define(spell_aura_mod_healing_pct, 118).
-define(spell_aura_share_pet_tracking, 119).
-define(spell_aura_untrackable, 120).
-define(spell_aura_empathy, 121).
-define(spell_aura_mod_offhand_damage_pct, 122).
-define(spell_aura_mod_target_resistance, 123).
-define(spell_aura_mod_ranged_attack_power, 124).
-define(spell_aura_mod_melee_damage_taken, 125).
-define(spell_aura_mod_melee_damage_taken_pct, 126).
-define(spell_aura_ranged_attack_power_attacker_bonus, 127).
-define(spell_aura_mod_possess_pet, 128).
-define(spell_aura_mod_speed_always, 129).
-define(spell_aura_mod_mounted_speed_always, 130).
-define(spell_aura_mod_ranged_attack_power_versus, 131).
-define(spell_aura_mod_increase_energy_percent, 132).
-define(spell_aura_mod_increase_health_percent, 133).
-define(spell_aura_mod_mana_regen_interrupt, 134).
-define(spell_aura_mod_healing_done, 135).
-define(spell_aura_mod_healing_done_percent, 136).
-define(spell_aura_mod_total_stat_percentage, 137).
-define(spell_aura_mod_melee_haste, 138).
-define(spell_aura_force_reaction, 139).
-define(spell_aura_mod_ranged_haste, 140).
-define(spell_aura_mod_ranged_ammo_haste, 141).
-define(spell_aura_mod_base_resistance_pct, 142).
-define(spell_aura_mod_resistance_exclusive, 143).
-define(spell_aura_safe_fall, 144).
-define(spell_aura_charisma, 145).
-define(spell_aura_persuaded, 146).
-define(spell_aura_mechanic_immunity_mask, 147).
-define(spell_aura_retain_combo_points, 148).
-define(spell_aura_resist_pushback , 149).
-define(spell_aura_mod_shield_blockvalue_pct, 150).
-define(spell_aura_track_stealthed , 151).
-define(spell_aura_mod_detected_range, 152).
-define(spell_aura_split_damage_flat, 153).
-define(spell_aura_mod_stealth_level, 154).
-define(spell_aura_mod_water_breathing, 155).
-define(spell_aura_mod_reputation_gain, 156).
-define(spell_aura_pet_damage_multi, 157).
-define(spell_aura_mod_shield_blockvalue, 158).
-define(spell_aura_no_pvp_credit, 159).
-define(spell_aura_mod_aoe_avoidance, 160).
-define(spell_aura_mod_health_regen_in_combat, 161).
-define(spell_aura_power_burn_mana, 162).
-define(spell_aura_mod_crit_damage_bonus, 163).
-define(spell_aura_164, 164).
-define(spell_aura_melee_attack_power_attacker_bonus, 165).
-define(spell_aura_mod_attack_power_pct, 166).
-define(spell_aura_mod_ranged_attack_power_pct, 167).
-define(spell_aura_mod_damage_done_versus, 168).
-define(spell_aura_mod_crit_percent_versus, 169).
-define(spell_aura_detect_amore, 170).
-define(spell_aura_mod_speed_not_stack, 171).
-define(spell_aura_mod_mounted_speed_not_stack, 172).
-define(spell_aura_allow_champion_spells, 173).
-define(spell_aura_mod_spell_damage_of_stat_percent, 174).
-define(spell_aura_mod_spell_healing_of_stat_percent, 175).
-define(spell_aura_spirit_of_redemption, 176).
-define(spell_aura_aoe_charm, 177).
-define(spell_aura_mod_debuff_resistance, 178).
-define(spell_aura_mod_attacker_spell_crit_chance, 179).
-define(spell_aura_mod_flat_spell_damage_versus, 180).
-define(spell_aura_mod_flat_spell_crit_damage_versus, 181).
-define(spell_aura_mod_resistance_of_stat_percent, 182).
-define(spell_aura_mod_critical_threat, 183).
-define(spell_aura_mod_attacker_melee_hit_chance, 184).
-define(spell_aura_mod_attacker_ranged_hit_chance, 185).
-define(spell_aura_mod_attacker_spell_hit_chance, 186).
-define(spell_aura_mod_attacker_melee_crit_chance, 187).
-define(spell_aura_mod_attacker_ranged_crit_chance, 188).
-define(spell_aura_mod_rating, 189).
-define(spell_aura_mod_faction_reputation_gain, 190).
-define(spell_aura_use_normal_movement_speed, 191).



















-define(spell_attr_unk0, 16#00000001).
-define(spell_attr_ranged, 16#00000002).
-define(spell_attr_on_next_swing_1, 16#00000004).
-define(spell_attr_unk3, 16#00000008).
-define(spell_attr_unk4, 16#00000010).
-define(spell_attr_tradespell, 16#00000020).
-define(spell_attr_passive, 16#00000040).
-define(spell_attr_hide_spell, 16#00000080).
-define(spell_attr_unk8, 16#00000100).
-define(spell_attr_unk9, 16#00000200).
-define(spell_attr_on_next_swing_2, 16#00000400).
-define(spell_attr_unk11, 16#00000800).
-define(spell_attr_daytime_only, 16#00001000).
-define(spell_attr_night_only, 16#00002000).
-define(spell_attr_indoors_only, 16#00004000).
-define(spell_attr_outdoors_only, 16#00008000).
-define(spell_attr_not_shapeshift, 16#00010000).
-define(spell_attr_only_stealthed, 16#00020000).
-define(spell_attr_unk18, 16#00040000).
-define(spell_attr_level_damage_calculation, 16#00080000).
-define(spell_attr_stop_attack_target, 16#00100000).
-define(spell_attr_impossible_dodge_parry_block, 16#00200000).
-define(spell_attr_set_tracking_target, 16#00400000).
-define(spell_attr_unk23, 16#00800000).
-define(spell_attr_castable_while_mounted, 16#01000000).
-define(spell_attr_disabled_while_active, 16#02000000).
-define(spell_attr_unk26, 16#04000000).
-define(spell_attr_castable_while_sitting, 16#08000000).
-define(spell_attr_cant_used_in_combat, 16#10000000).
-define(spell_attr_unaffected_by_invulnerability, 16#20000000).
-define(spell_attr_unk30, 16#40000000).
-define(spell_attr_cant_cancel, 16#80000000).


-define(spell_attr_ex_unk0, 16#00000001).
-define(spell_attr_ex_drain_all_power, 16#00000002).
-define(spell_attr_ex_channeled_1, 16#00000004).
-define(spell_attr_ex_unk3, 16#00000008).
-define(spell_attr_ex_unk4, 16#00000010).
-define(spell_attr_ex_not_break_stealth, 16#00000020).
-define(spell_attr_ex_channeled_2, 16#00000040).
-define(spell_attr_ex_negative, 16#00000080).
-define(spell_attr_ex_not_in_combat_target, 16#00000100).
-define(spell_attr_ex_unk9, 16#00000200).
-define(spell_attr_ex_no_threat, 16#00000400).
-define(spell_attr_ex_unk11, 16#00000800).
-define(spell_attr_ex_unk12, 16#00001000).
-define(spell_attr_ex_farsight, 16#00002000).
-define(spell_attr_ex_unk14, 16#00004000).
-define(spell_attr_ex_dispel_auras_on_immunity, 16#00008000).
-define(spell_attr_ex_unaffected_by_school_immune, 16#00010000).
-define(spell_attr_ex_unk17, 16#00020000).
-define(spell_attr_ex_unk18, 16#00040000).
-define(spell_attr_ex_unk19, 16#00080000).
-define(spell_attr_ex_req_target_combo_points, 16#00100000).
-define(spell_attr_ex_unk21, 16#00200000).
-define(spell_attr_ex_req_combo_points, 16#00400000).
-define(spell_attr_ex_unk23, 16#00800000).
-define(spell_attr_ex_unk24, 16#01000000).
-define(spell_attr_ex_unk25, 16#02000000).
-define(spell_attr_ex_unk26, 16#04000000).
-define(spell_attr_ex_unk27, 16#08000000).
-define(spell_attr_ex_unk28, 16#10000000).
-define(spell_attr_ex_unk29, 16#20000000).
-define(spell_attr_ex_unk30, 16#40000000).
-define(spell_attr_ex_unk31, 16#80000000).


-define(spell_attr_ex2_unk0, 16#00000001).
-define(spell_attr_ex2_unk1, 16#00000002).
-define(spell_attr_ex2_cant_reflected, 16#00000004).
-define(spell_attr_ex2_unk3, 16#00000008).
-define(spell_attr_ex2_unk4, 16#00000010).
-define(spell_attr_ex2_autorepeat_flag, 16#00000020).
-define(spell_attr_ex2_unk6, 16#00000040).
-define(spell_attr_ex2_unk7, 16#00000080).
-define(spell_attr_ex2_unk8, 16#00000100).
-define(spell_attr_ex2_unk9, 16#00000200).
-define(spell_attr_ex2_unk10, 16#00000400).
-define(spell_attr_ex2_health_funnel, 16#00000800).
-define(spell_attr_ex2_unk12, 16#00001000).
-define(spell_attr_ex2_unk13, 16#00002000).
-define(spell_attr_ex2_unk14, 16#00004000).
-define(spell_attr_ex2_unk15, 16#00008000).
-define(spell_attr_ex2_unk16, 16#00010000).
-define(spell_attr_ex2_unk17, 16#00020000).
-define(spell_attr_ex2_unk18, 16#00040000).
-define(spell_attr_ex2_not_need_shapeshift, 16#00080000).
-define(spell_attr_ex2_unk20, 16#00100000).
-define(spell_attr_ex2_damage_reduced_shield, 16#00200000).
-define(spell_attr_ex2_unk22, 16#00400000).
-define(spell_attr_ex2_unk23, 16#00800000).
-define(spell_attr_ex2_unk24, 16#01000000).
-define(spell_attr_ex2_unk25, 16#02000000).
-define(spell_attr_ex2_unk26, 16#04000000).
-define(spell_attr_ex2_unk27, 16#08000000).
-define(spell_attr_ex2_unk28, 16#10000000).
-define(spell_attr_ex2_cant_crit, 16#20000000).
-define(spell_attr_ex2_unk30, 16#40000000).
-define(spell_attr_ex2_food_buff, 16#80000000).


-define(spell_attr_ex3_unk0, 16#00000001).
-define(spell_attr_ex3_unk1, 16#00000002).
-define(spell_attr_ex3_unk2, 16#00000004).
-define(spell_attr_ex3_unk3, 16#00000008).
-define(spell_attr_ex3_unk4, 16#00000010).
-define(spell_attr_ex3_unk5, 16#00000020).
-define(spell_attr_ex3_unk6, 16#00000040).
-define(spell_attr_ex3_unk7, 16#00000080).
-define(spell_attr_ex3_target_only_player, 16#00000100).
-define(spell_attr_ex3_unk9, 16#00000200).
-define(spell_attr_ex3_main_hand, 16#00000400).
-define(spell_attr_ex3_battleground, 16#00000800).
-define(spell_attr_ex3_cast_on_dead, 16#00001000).
-define(spell_attr_ex3_unk13, 16#00002000).
-define(spell_attr_ex3_unk14, 16#00004000).
-define(spell_attr_ex3_unk15, 16#00008000).
-define(spell_attr_ex3_unk16, 16#00010000).
-define(spell_attr_ex3_no_initial_aggro, 16#00020000).
-define(spell_attr_ex3_cant_miss, 16#00040000).
-define(spell_attr_ex3_unk19, 16#00080000).
-define(spell_attr_ex3_death_persistent, 16#00100000).
-define(spell_attr_ex3_unk21, 16#00200000).
-define(spell_attr_ex3_req_wand, 16#00400000).
-define(spell_attr_ex3_unk23, 16#00800000).
-define(spell_attr_ex3_req_offhand, 16#01000000).
-define(spell_attr_ex3_unk25, 16#02000000).
-define(spell_attr_ex3_unk26, 16#04000000).
-define(spell_attr_ex3_unk27, 16#08000000).
-define(spell_attr_ex3_unk28, 16#10000000).
-define(spell_attr_ex3_unk29, 16#20000000).
-define(spell_attr_ex3_unk30, 16#40000000).
-define(spell_attr_ex3_unk31, 16#80000000).


-define(spell_attr_ex4_unk0, 16#00000001).
-define(spell_attr_ex4_unk1, 16#00000002).
-define(spell_attr_ex4_unk2, 16#00000004).
-define(spell_attr_ex4_unk3, 16#00000008).
-define(spell_attr_ex4_unk4, 16#00000010).
-define(spell_attr_ex4_unk5, 16#00000020).
-define(spell_attr_ex4_not_stealable, 16#00000040).
-define(spell_attr_ex4_unk7, 16#00000080).
-define(spell_attr_ex4_unk8, 16#00000100).
-define(spell_attr_ex4_unk9, 16#00000200).
-define(spell_attr_ex4_spell_vs_extend_cost, 16#00000400).
-define(spell_attr_ex4_unk11, 16#00000800).
-define(spell_attr_ex4_unk12, 16#00001000).
-define(spell_attr_ex4_unk13, 16#00002000).
-define(spell_attr_ex4_unk14, 16#00004000).
-define(spell_attr_ex4_unk15, 16#00008000).
-define(spell_attr_ex4_not_usable_in_arena, 16#00010000).
-define(spell_attr_ex4_usable_in_arena, 16#00020000).
-define(spell_attr_ex4_unk18, 16#00040000).
-define(spell_attr_ex4_unk19, 16#00080000).
-define(spell_attr_ex4_unk20, 16#00100000).
-define(spell_attr_ex4_unk21, 16#00200000).
-define(spell_attr_ex4_unk22, 16#00400000).
-define(spell_attr_ex4_unk23, 16#00800000).
-define(spell_attr_ex4_unk24, 16#01000000).
-define(spell_attr_ex4_unk25, 16#02000000).
-define(spell_attr_ex4_cast_only_in_outland, 16#04000000).
-define(spell_attr_ex4_unk27, 16#08000000).
-define(spell_attr_ex4_unk28, 16#10000000).
-define(spell_attr_ex4_unk29, 16#20000000).
-define(spell_attr_ex4_unk30, 16#40000000).
-define(spell_attr_ex4_unk31, 16#80000000).
