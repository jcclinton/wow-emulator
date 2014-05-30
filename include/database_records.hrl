-record(account, {name, salt, verifier}).
-record(realm,   {id, name, icon, lock, status, address, population, timezone}).
-record(char,    {guid, account_id, name, race, gender, level, xp, money, playerBytes,
									playerBytes2, playerFlags, position_x, position_y, position_z, map,
									orientation, taximask, cinematic, totaltime, leveltime, rest_bonus,
									logout_time, is_logout_resting, resettalents_cost, resettalents_time,
									trans_x, trans_y, trans_z, trans_o, transguid, extra_flags, stable_slots,
									at_login, zone, online, death_expire_time, taxi_path, honor_highest_rank,
									honor_standing, stored_honor_rating, stored_dishonorablekills,
									stored_honorable_kills, watchedFaction, drunk, health, power1, power2,
									power3, power4, power5, exploredZones, equipmentCache, ammoId, actionBars}).
-record(char_old,    {id, account_id, realm_id, name, race, gender, class, power_type,
                  skin, face, hair_style, hair_color, facial_hair, outfit_id, level, 
                  guild_id, general_flags, at_login_flags, faction_template, 
                  map_id, zone_id, position_x, position_y, position_z, orientation, 
                  display_id, strength, agility, stamina, intellect, spirit, 
                  health, mana, focus, power, intro, attack_power, min_dmg, 
                  max_dmg, scale}).
-record(pet,     {id, owner}).

-record(char_create_info, {faction_template, map_id, zone_id, position_x, 
                           position_y, position_z, orientation, display_id, 
                           strength, agility, stamina, intellect, spirit, 
                           health, mana, focus, power, power_type, intro,
                           attack_power, min_dmg, max_dmg, scale}).
