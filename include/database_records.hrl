-record(account, {name, salt, verifier}).
-record(realm,   {id, name, icon, lock, status, address, population, timezone}).

-record(char,    {id, account_id, realm_id, name, race, gender, class, power_type,
                  skin, face, hair_style, hair_color, facial_hair, outfit_id, level, 
                  guild_id, general_flags, at_login_flags, faction_template, 
                  map_id, zone_id, position_x, position_y, position_z, orientation, 
                  display_id, strength, agility, stamina, intellect, spirit, 
                  health, mana, focus, power, intro, attack_power, min_dmg, 
                  max_dmg, scale, model_id}).
-record(pet,     {id, owner}).

%char session is not persistent
-record(char_sess, {target=0}).

-record(char_create_info, {faction_template, map_id, zone_id, position_x, 
                           position_y, position_z, orientation, display_id, 
                           strength, agility, stamina, intellect, spirit, 
                           health, mana, focus, power, power_type, intro,
                           attack_power, min_dmg, max_dmg, scale}).
