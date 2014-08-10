-define(power_mana, 0).
-define(power_rage, 1).
-define(power_focus, 2).
-define(power_energy, 3).
-define(power_happiness, 4).
-define(power_all, 127).
-define(power_health, 16#FFFFFFFE).
-define(max_powers, 5).


-define(player_slot_start, 0).
-define(player_slot_end, 118).
-define(player_slots_count, 118).

-define(inventory_slot_bag_0, 255).

		%19 slots
-define(equipment_slot_start, 0).
-define(equipment_slot_head, 0).
-define(equipment_slot_neck, 1).
-define(equipment_slot_shoulders, 2).
-define(equipment_slot_body, 3).
-define(equipment_slot_chest, 4).
-define(equipment_slot_waist, 5).
-define(equipment_slot_legs, 6).
-define(equipment_slot_feet, 7).
-define(equipment_slot_wrists, 8).
-define(equipment_slot_hands, 9).
-define(equipment_slot_finger1, 10).
-define(equipment_slot_finger2, 11).
-define(equipment_slot_trinket1, 12).
-define(equipment_slot_trinket2, 13).
-define(equipment_slot_back, 14).
-define(equipment_slot_mainhand, 15).
-define(equipment_slot_offhand, 16).
-define(equipment_slot_ranged, 17).
-define(equipment_slot_tabard, 18).
-define(equipment_slot_end, 19).

		% 4 slots
-define(inventory_slot_bag_start, 19).
-define(inventory_slot_bag_end, 23).

		%16 slots
-define(inventory_slot_item_start, 23).
-define(inventory_slot_item_end, 39).

		% 28 slots
-define(bank_slot_item_start, 39).
-define(bank_slot_item_end, 63).

		% 7 slots
-define(bank_slot_bag_start, 63).
-define(bank_slot_bag_end, 69).

		%12 slots
-define(buyback_slot_start, 69).
-define(buyback_slot_end, 81).

		%32 slots
-define(keyring_slot_start, 81).
-define(keyring_slot_end, 97).






% used in the PLAYER_FLAGS field
-define(player_flags_none, 16#00000000).
-define(player_flags_group_leader, 16#00000001).
-define(player_flags_afk, 16#00000002).
-define(player_flags_dnd, 16#00000004).
-define(player_flags_gm, 16#00000008).
-define(player_flags_ghost, 16#00000010).
-define(player_flags_resting, 16#00000020).
-define(player_flags_unk7, 16#00000040).       %% admin?
-define(player_flags_ffa_pvp, 16#00000080).
-define(player_flags_contested_pvp, 16#00000100).       %% Player has been involved in a PvP combat and will be attacked by contested guards
-define(player_flags_in_pvp, 16#00000200).
-define(player_flags_hide_helm, 16#00000400).
-define(player_flags_hide_cloak, 16#00000800).
-define(player_flags_partial_play_time, 16#00001000).       %% played long time
-define(player_flags_no_play_time, 16#00002000).       %% played too long time
-define(player_flags_unk15, 16#00004000).
-define(player_flags_unk16, 16#00008000).       %% strange visual effect (2.0.1), looks like PLAYER_FLAGS_GHOST flag
-define(player_flags_sanctuary, 16#00010000).       %% player entered sanctuary
-define(player_flags_taxi_benchmark, 16#00020000).       %% taxi benchmark mode (on/off) (2.0.1)
-define(player_flags_pvp_timer, 16#00040000).       %% 3.0.2, pvp timer active (after you disable pvp manually)
