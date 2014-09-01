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

-define(max_visible_item_offset, 12).
-define(default_item_stack_size, 5).

-define(item_class_consumable, 0).
-define(item_class_container, 1).
-define(item_class_weapon, 2).
-define(item_class_reserved_1, 3).
-define(item_class_armor, 4).
-define(item_class_reagent, 5).
-define(item_class_projectile, 6).
-define(item_class_trade_goods, 7).
-define(item_class_reserved_2, 8).
-define(item_class_recipe, 9).
-define(item_class_reserved_3, 10).
-define(item_class_quiver, 11).
-define(item_class_quest, 12).
-define(item_class_key, 13).
-define(item_class_reserved_4, 14).
-define(item_class_misc, 15).


-define(invtype_non_equip, 0).
-define(invtype_head, 1).
-define(invtype_neck, 2).
-define(invtype_shoulders, 3).
-define(invtype_body, 4).
-define(invtype_chest, 5).
-define(invtype_waist, 6).
-define(invtype_legs, 7).
-define(invtype_feet, 8).
-define(invtype_wrists, 9).
-define(invtype_hands, 10).
-define(invtype_finger, 11).
-define(invtype_trinket, 12).
-define(invtype_weapon, 13).
-define(invtype_shield, 14).
-define(invtype_ranged, 15).
-define(invtype_cloak, 16).
-define(invtype_2hweapon, 17).
-define(invtype_bag, 18).
-define(invtype_tabard, 19).
-define(invtype_robe, 20).
-define(invtype_weaponmainhand, 21).
-define(invtype_weaponoffhand, 22).
-define(invtype_holdable, 23).
-define(invtype_ammo, 24).
-define(invtype_thrown, 25).
-define(invtype_rangedright, 26).
-define(invtype_quiver, 27).
-define(invtype_relic, 28).



-define(equip_err_ok, 0).
-define(equip_err_cant_equip_level_i, 1).
-define(equip_err_cant_equip_skill, 2).
-define(equip_err_item_doesnt_go_to_slot, 3).
-define(equip_err_bag_full, 4).
-define(equip_err_nonempty_bag_over_other_bag, 5).
-define(equip_err_cant_trade_equip_bags, 6).
-define(equip_err_only_ammo_can_go_here, 7).
-define(equip_err_no_required_proficiency, 8).
-define(equip_err_no_equipment_slot_available, 9).
-define(equip_err_you_can_never_use_that_item, 10).
-define(equip_err_you_can_never_use_that_item2, 11).
-define(equip_err_no_equipment_slot_available2, 12).
-define(equip_err_cant_equip_with_twohanded, 13).
-define(equip_err_cant_dual_wield, 14).
-define(equip_err_item_doesnt_go_into_bag, 15).
-define(equip_err_item_doesnt_go_into_bag2, 16).
-define(equip_err_cant_carry_more_of_this, 17).
-define(equip_err_no_equipment_slot_available3, 18).
-define(equip_err_item_cant_stack, 19).
-define(equip_err_item_cant_be_equipped, 20).
-define(equip_err_items_cant_be_swapped, 21).
-define(equip_err_slot_is_empty, 22).
-define(equip_err_item_not_found, 23).
-define(equip_err_cant_drop_soulbound, 24).
-define(equip_err_out_of_range, 25).
-define(equip_err_tried_to_split_more_than_count, 26).
-define(equip_err_couldnt_split_items, 27).
-define(equip_err_missing_reagent, 28).
-define(equip_err_not_enough_money, 29).
-define(equip_err_not_a_bag, 30).
-define(equip_err_can_only_do_with_empty_bags, 31).
-define(equip_err_dont_own_that_item, 32).
-define(equip_err_can_equip_only1_quiver, 33).
-define(equip_err_must_purchase_that_bag_slot, 34).
-define(equip_err_too_far_away_from_bank, 35).
-define(equip_err_item_locked, 36).
-define(equip_err_you_are_stunned, 37).
-define(equip_err_you_are_dead, 38).
-define(equip_err_cant_do_right_now, 39).
-define(equip_err_int_bag_error, 40).
-define(equip_err_can_equip_only1_bolt, 41).
-define(equip_err_can_equip_only1_ammopouch, 42).
-define(equip_err_stackable_cant_be_wrapped, 43).
-define(equip_err_equipped_cant_be_wrapped, 44).
-define(equip_err_wrapped_cant_be_wrapped, 45).
-define(equip_err_bound_cant_be_wrapped, 46).
-define(equip_err_unique_cant_be_wrapped, 47).
-define(equip_err_bags_cant_be_wrapped, 48).
-define(equip_err_already_looted, 49).
-define(equip_err_inventory_full, 50).
-define(equip_err_bank_full, 51).
-define(equip_err_item_is_currently_sold_out, 52).
-define(equip_err_bag_full3, 53).
-define(equip_err_item_not_found2, 54).
-define(equip_err_item_cant_stack2, 55).
-define(equip_err_bag_full4, 56).
-define(equip_err_item_sold_out, 57).
-define(equip_err_object_is_busy, 58).
-define(equip_err_none, 59).
-define(equip_err_not_in_combat, 60).
-define(equip_err_not_while_disarmed, 61).
-define(equip_err_bag_full6, 62).
-define(equip_err_cant_equip_rank, 63).
-define(equip_err_cant_equip_reputation, 64).
-define(equip_err_too_many_special_bags, 65).
-define(equip_err_loot_cant_loot_that_now, 66).

-define(buy_err_cant_find_item, 0).
-define(buy_err_item_already_sold, 1).
-define(buy_err_not_enought_money, 2).
-define(buy_err_seller_dont_like_you, 4).
-define(buy_err_distance_too_far, 5).
-define(buy_err_item_sold_out, 7).
-define(buy_err_cant_carry_more, 8).
-define(buy_err_rank_require, 11).
-define(buy_err_reputation_require, 12).

-define(sell_err_cant_find_item, 1,
-define(sell_err_cant_sell_item, 2).
-define(sell_err_cant_find_vendor, 3).
-define(sell_err_you_dont_own_that_item, 4).
-define(sell_err_unk, 5).
-define(sell_err_only_empty_bag, 6).
