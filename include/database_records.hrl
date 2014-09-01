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

% account is used to store a users account data
-record(account, {name, salt, verifier}).

% char is the namespace of a characters persistent data store
-record(char_move, {x, y, z, orient, zone, map, movement_info}).

-record(char_misc, {at_login_flags}).

-record(char_spells, {ids=[]}).

%char session is not persistent
-record(char_sess, {}).

% char create info is used in initializing a new character
-record(char_create_info, {faction_template, map_id, zone_id, position_x, 
                           position_y, position_z, orientation, display_id, 
                           strength, agility, stamina, intellect, spirit, 
                           health, mana, focus, power, power_type, intro,
                           attack_power, min_dmg, max_dmg,
													 initial_spells, initial_action_bars}).


% item record
-record(item_proto, {
	id,
	class,
	sub_class,
	stackable,
	stat_type1,
	stat_value1,
	stat_type2,
	stat_value2,
	stat_type3,
	stat_value3,
	stat_type4,
	stat_value4,
	stat_type5,
	stat_value5,
	stat_type6,
	stat_value6,
	stat_type7,
	stat_value7,
	stat_type8,
	stat_value8,
	stat_type9,
	stat_value9,
	stat_type10,
	stat_value10,

	dmg_min1,
	dmg_max1,
	dmg_type1,
	dmg_min2,
	dmg_max2,
	dmg_type2,
	dmg_min3,
	dmg_max3,
	dmg_type3,
	dmg_min4,
	dmg_max4,
	dmg_type4,
	dmg_min5,
	dmg_max5,
	dmg_type5,

	armor,
	holy_res,
	fire_res,
	nature_res,
	frost_res,
	shadow_res,
	arcane_res,

	delay,

	block,

	max_durability,
	display_info_id,
	inventory_type
}).

% static object stores

-record(item_class_store, {id, name}).

-record(spell_store, {
	id,
	school,
	category,
	dispel,
	mechanic,
	attributes,
	attributes_ex,
	attributes_ex2,
	attributes_ex3,
	attributes_ex4,
	stances,
	stances_not,
	targets,
	target_creature_type,
	requires_spell_focus,
	caster_aura_state,
	target_aura_state,
	casting_time_index,
	recovery_time,
	category_recovery_time,
	interrupt_flags,
	aura_interrupt_flags,
	channel_interrupt_flags,
	proc_flags,
	proc_chance,
	proc_charges,
	max_level,
	base_level,
	spell_level,
	duration_index,
	power_type,
	mana_cost,
	mana_cost_perlevel,
	mana_per_second,
	mana_per_second_per_level,
	range_index,
	speed,
	modal_next_spell,
	stack_amount,
	totem,
	reagent,
	reagent_count,
	equipped_item_class,
	equipped_item_sub_class_mask,
	equipped_item_inventory_type_mask,
	effects,
	spell_visual,
	spell_icon_id,
	active_icon_id,
	spell_name,
	rank,
	mana_cost_percentage,
	start_recovery_category,
	start_recovery_time,
	max_target_level,
	spell_family_name,
	spell_family_flags,
	max_affected_targets,
	dmg_class,
	prevention_type,
	dmg_multiplier}).


-record(spell_effect, {
	effect,
	effect_die_sides,
	effect_base_dice,
	effect_dice_per_level,
	effect_real_points_per_level,
	effect_base_points,
	effect_mechanic,
	effect_implicit_target_a,
	effect_implicit_target_b,
	effect_radius_index,
	effect_apply_aura_name,
	effect_amplitude,
	effect_multiple_value,
	effect_chain_target,
	effect_item_type,
	effect_misc_value,
	effect_trigger_spell,
	effect_points_per_combo_point
}).
