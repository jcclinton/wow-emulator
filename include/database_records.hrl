% account is used to store a users account data
-record(account, {name, salt, verifier}).

% char is the persistent data store
-record(char, {x, y, z, orient, name, zone, map, at_login_flags}).

%char session is not persistent
-record(char_sess, {target=0, update_mask}).

% char create info is used in initializing a new character
-record(char_create_info, {faction_template, map_id, zone_id, position_x, 
                           position_y, position_z, orientation, display_id, 
                           strength, agility, stamina, intellect, spirit, 
                           health, mana, focus, power, power_type, intro,
                           attack_power, min_dmg, max_dmg, scale}).



% static object stores

%area
-record(area_store, {id, map_id, zone_id, explore_flag, flags, area_level, area_name, team, liqueid_type_override}).
