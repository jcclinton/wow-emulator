% account is used to store a users account data
-record(account, {name, salt, verifier}).

% char is the namespace of a characters persistent data store
-record(char_move, {x, y, z, orient, zone, map, movement_info}).

-record(char_misc, {at_login_flags}).

-record(char_spells, {ids=[]}).

%char session is not persistent
-record(char_sess, {target=0, update_mask}).

% char create info is used in initializing a new character
-record(char_create_info, {faction_template, map_id, zone_id, position_x, 
                           position_y, position_z, orientation, display_id, 
                           strength, agility, stamina, intellect, spirit, 
                           health, mana, focus, power, power_type, intro,
                           attack_power, min_dmg, max_dmg, scale,
													 initial_spells, initial_action_bars}).



% static object stores

-record(item_class_store, {id, name}).

-record(char_start_outfit_store, {race, class, gender, item_ids}).

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
	effect_points_per_combo_point,
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
