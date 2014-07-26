-module(content).
-export([char_create_info/2]).

-include("include/database_records.hrl").

%% @type race() = human | orc | dwarf | night_elf |
%%                undead | tauren | gnome | troll |
%%                blood_elf | draenei.
%% @type class() = warrior | paladin | hunter | rogue |
%%                 priest | death_knight | shaman |
%%                 mage | warlock | druid.

%% @spec char_create_info(race(), class()) -> tuple().
char_create_info(Race, Class) -> 
    CI1 = race_create_info(Race, #char_create_info{}),
    CI2 = class_create_info(Class, CI1),
    CI3 = race_class_create_info(Race, Class, CI2),
		Spells = initial_spells(Race, Class),
		ActionBars = initial_action_bars(Race, Class),
		CI3#char_create_info{initial_spells=Spells, initial_action_bars=ActionBars}.


%% @spec race_class_create_info(race(), class(), tuple()) -> tuple().
race_class_create_info(human,     warrior, Rec) -> Rec#char_create_info{health = 60, power = 100};
race_class_create_info(orc,       warrior, Rec) -> Rec#char_create_info{health = 80, power = 100};
race_class_create_info(dwarf,     warrior, Rec) -> Rec#char_create_info{health = 90, power = 100};
race_class_create_info(night_elf, warrior, Rec) -> Rec#char_create_info{health = 50, power = 100};
race_class_create_info(undead,    warrior, Rec) -> Rec#char_create_info{health = 70, power = 100};
race_class_create_info(tauren,    warrior, Rec) -> Rec#char_create_info{health = 80, power = 100};
race_class_create_info(gnome,     warrior, Rec) -> Rec#char_create_info{health = 50, power = 100};
race_class_create_info(troll,     warrior, Rec) -> Rec#char_create_info{health = 70, power = 100};
race_class_create_info(blood_elf, warrior, Rec) -> Rec#char_create_info{health = 50, power = 100};
race_class_create_info(draenei,   warrior, Rec) -> Rec#char_create_info{health = 70, power = 100};

race_class_create_info(human,     paladin, Rec) -> Rec#char_create_info{health = 58, power = 80};
race_class_create_info(dwarf,     paladin, Rec) -> Rec#char_create_info{health = 88, power = 79};
race_class_create_info(blood_elf, paladin, Rec) -> Rec#char_create_info{health = 38, power = 140};
race_class_create_info(draenei,   paladin, Rec) -> Rec#char_create_info{health = 48, power = 95};

race_class_create_info(orc,       hunter, Rec) -> Rec#char_create_info{health = 76, power = 82};
race_class_create_info(dwarf,     hunter, Rec) -> Rec#char_create_info{health = 86, power = 84};
race_class_create_info(night_elf, hunter, Rec) -> Rec#char_create_info{health = 46, power = 85};
race_class_create_info(tauren,    hunter, Rec) -> Rec#char_create_info{health = 76, power = 80};
race_class_create_info(troll,     hunter, Rec) -> Rec#char_create_info{health = 66, power = 81};
race_class_create_info(blood_elf, hunter, Rec) -> Rec#char_create_info{health = 45, power = 145};
race_class_create_info(draenei,   hunter, Rec) -> Rec#char_create_info{health = 46, power = 100};

race_class_create_info(human,     rogue, Rec) -> Rec#char_create_info{health = 55, power = 100};
race_class_create_info(orc,       rogue, Rec) -> Rec#char_create_info{health = 75, power = 100};
race_class_create_info(dwarf,     rogue, Rec) -> Rec#char_create_info{health = 85, power = 100};
race_class_create_info(night_elf, rogue, Rec) -> Rec#char_create_info{health = 45, power = 100};
race_class_create_info(undead,    rogue, Rec) -> Rec#char_create_info{health = 65, power = 100};
race_class_create_info(gnome,     rogue, Rec) -> Rec#char_create_info{health = 45, power = 100};
race_class_create_info(troll,     rogue, Rec) -> Rec#char_create_info{health = 65, power = 100};
race_class_create_info(blood_elf, rogue, Rec) -> Rec#char_create_info{health = 44, power = 100};

race_class_create_info(human,     priest, Rec) -> Rec#char_create_info{health = 52, power = 160};
race_class_create_info(dwarf,     priest, Rec) -> Rec#char_create_info{health = 82, power = 145};
race_class_create_info(night_elf, priest, Rec) -> Rec#char_create_info{health = 51, power = 160};
race_class_create_info(undead,    priest, Rec) -> Rec#char_create_info{health = 62, power = 130};
race_class_create_info(troll,     priest, Rec) -> Rec#char_create_info{health = 62, power = 128};
race_class_create_info(blood_elf, priest, Rec) -> Rec#char_create_info{health = 50, power = 220};
race_class_create_info(draenei,   priest, Rec) -> Rec#char_create_info{health = 51, power = 175};

% TODO: find death knight start values
race_class_create_info(human,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(orc,       death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(dwarf,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(night_elf, death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(undead,    death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(tauren,    death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(gnome,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(troll,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(blood_elf, death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(draenei,   death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};

race_class_create_info(orc,       shaman, Rec) -> Rec#char_create_info{health = 77, power = 73};
race_class_create_info(tauren,    shaman, Rec) -> Rec#char_create_info{health = 77, power = 71};
race_class_create_info(troll,     shaman, Rec) -> Rec#char_create_info{health = 67, power = 72};
race_class_create_info(draenei,   shaman, Rec) -> Rec#char_create_info{health = 47, power = 105};

race_class_create_info(human,     mage, Rec) -> Rec#char_create_info{health = 52, power = 165};
race_class_create_info(undead,    mage, Rec) -> Rec#char_create_info{health = 62, power = 135};
race_class_create_info(gnome,     mage, Rec) -> Rec#char_create_info{health = 51, power = 210};
race_class_create_info(troll,     mage, Rec) -> Rec#char_create_info{health = 62, power = 119};
race_class_create_info(blood_elf, mage, Rec) -> Rec#char_create_info{health = 50, power = 225};
race_class_create_info(draenei,   mage, Rec) -> Rec#char_create_info{health = 51, power = 180};

race_class_create_info(human,     warlock, Rec) -> Rec#char_create_info{health = 53, power = 140};
race_class_create_info(orc,       warlock, Rec) -> Rec#char_create_info{health = 73, power = 109};
race_class_create_info(undead,    warlock, Rec) -> Rec#char_create_info{health = 63, power = 110};
race_class_create_info(gnome,     warlock, Rec) -> Rec#char_create_info{health = 43, power = 185};
race_class_create_info(blood_elf, warlock, Rec) -> Rec#char_create_info{health = 42, power = 200};

race_class_create_info(night_elf, druid, Rec) -> Rec#char_create_info{health = 53, power = 100};
race_class_create_info(tauren,    druid, Rec) -> Rec#char_create_info{health = 74, power = 67};
race_class_create_info(_, _, _) ->
    wrong_race_class.

%% @spec race_create_info(race(), tuple()) -> tuple().
race_create_info(human, Rec) ->
    Rec#char_create_info{faction_template = 1, 
                         display_id       = 49, 
                         scale            = 1, 
                         map_id           = 0, 
                         %zone_id          = 12, 
                         zone_id          = 33, 
                         intro            = 81,
                         position_x       = -11609.95, 
                         position_y       = -41.493, 
                         position_z       = 10.5312, 
                         %position_x       = -8949.95, 
                         %position_y       = -132.493, 
                         %position_z       = 83.5312, 
                         orientation      = 0,
                         agility          = 20, 
                         intellect        = 20, 
                         spirit           = 20, 
                         stamina          = 20, 
                         strength         = 20};
race_create_info(orc, Rec) ->
    Rec#char_create_info{faction_template = 2, 
                         display_id       = 51, 
                         scale            = 1, 
                         map_id           = 0, 
                         zone_id          = 33, 
												 % temporarily spawn orcs in elwynn with humans
                         %map_id           = 1, 
                         %zone_id          = 14, 
                         intro            = 21,
                         position_x       = -11609.95, 
                         position_y       = -41.493, 
                         position_z       = 10.5312, 
                         %position_x       = -618.518, 
                         %position_y       = -4251.67, 
                         %position_z       = 38.718, 
                         orientation      = 0,
                         agility          = 17, 
                         intellect        = 17, 
                         spirit           = 23, 
                         stamina          = 22, 
                         strength         = 23};
race_create_info(dwarf, Rec) ->
    Rec#char_create_info{faction_template = 3, 
                         display_id       = 53, 
                         scale            = 1, 
                         map_id           = 0, 
                         zone_id          = 1, 
                         intro            = 41,
                         position_x       = -6240.32,
                         position_y       = 331.033,
                         position_z       = 382.758, 
                         orientation      = 0,
                         agility          = 16, 
                         intellect        = 19, 
                         spirit           = 19, 
                         stamina          = 23, 
                         strength         = 22};
race_create_info(night_elf, Rec) ->
    Rec#char_create_info{faction_template = 4, 
                         display_id       = 55, 
                         scale            = 1, 
                         map_id           = 1, 
                         zone_id          = 141, 
                         intro            = 61,
                         position_x       = 10311.3, 
                         position_y       = 832.463, 
                         position_z       = 1326.41, 
                         orientation      = 0,
                         agility          = 25, 
                         intellect        = 20, 
                         spirit           = 20, 
                         stamina          = 19, 
                         strength         = 17};
race_create_info(undead, Rec) ->
    Rec#char_create_info{faction_template = 5, 
                         display_id       = 57, 
                         scale            = 1, 
                         map_id           = 0, 
                         zone_id          = 85, 
                         intro            = 2,
                         position_x       = 1676.35, 
                         position_y       = 1677.45, 
                         position_z       = 121.67, 
                         orientation      = 0,
                         agility          = 18, 
                         intellect        = 18, 
                         spirit           = 25, 
                         stamina          = 21, 
                         strength         = 19};
race_create_info(tauren, Rec) ->
    Rec#char_create_info{faction_template = 6, 
                         display_id       = 59, 
                         scale            = 1.3, 
                         map_id           = 1, 
                         zone_id          = 215, 
                         intro            = 141,
                         position_x       = -2917.58, 
                         position_y       = -257.98, 
                         position_z       = 52.9968, 
                         orientation      = 0,
                         agility          = 15, 
                         intellect        = 15, 
                         spirit           = 22, 
                         stamina          = 22, 
                         strength         = 25};
race_create_info(gnome, Rec) ->
    Rec#char_create_info{faction_template = 8, 
                         display_id       = 1563, 
                         scale            = 1, 
                         map_id           = 0, 
                         zone_id          = 1, 
                         intro            = 101,
                         position_x       = -6237.02, 
                         position_y       = 329.659, 
                         position_z       = 382.703, 
                         orientation      = 0,
                         agility          = 23, 
                         intellect        = 23, 
                         spirit           = 20, 
                         stamina          = 19, 
                         strength         = 15};
race_create_info(troll, Rec) ->
    Rec#char_create_info{faction_template = 9, 
                         display_id       = 1478, 
                         scale            = 1, 
                         map_id           = 1, 
                         zone_id          = 14, 
                         intro            = 121,
                         position_x       = -618.518, 
                         position_y       = -4251.67, 
                         position_z       = 38.718, 
                         orientation      = 0,
                         agility          = 22, 
                         intellect        = 16, 
                         spirit           = 21, 
                         stamina          = 21, 
                         strength         = 21};
race_create_info(blood_elf, Rec) ->
    Rec#char_create_info{faction_template = 914, 
                         display_id       = 15467, 
                         scale            = 1, 
                         map_id           = 530, 
                         zone_id          = 3430, 
                         intro            = 162,
                         position_x       = 10349.6, 
                         position_y       = -6357.29, 
                         position_z       = 33.4026, 
                         orientation      = 0,
                         agility          = 22, 
                         intellect        = 24, 
                         spirit           = 19, 
                         stamina          = 18, 
                         strength         = 17};
race_create_info(draenei, Rec) ->
    Rec#char_create_info{faction_template = 927, 
                         display_id       = 16125, 
                         scale            = 1, 
                         map_id           = 530, 
                         zone_id          = 3524, 
                         intro            = 163,
                         position_x       = -3961.64, 
                         position_y       = -13931.2, 
                         position_z       = 100.615, 
                         orientation      = 0,
                         agility          = 17, 
                         intellect        = 21, 
                         spirit           = 22, 
                         stamina          = 19, 
                         strength         = 21}.

%% @spec class_create_info(class(), tuple()) -> tuple().
class_create_info(warrior, Rec) ->
    Rec#char_create_info{agility    = 0.81 * Rec#char_create_info.agility, 
                         intellect  = 0.2  * Rec#char_create_info.intellect,
                         spirit     = 0.3  * Rec#char_create_info.spirit,
                         stamina    = 1.1  * Rec#char_create_info.stamina, 
                         strength   = 1.2  * Rec#char_create_info.strength, 
                         min_dmg    = 5,
                         max_dmg    = 7,
                         power_type = rage};
class_create_info(paladin, Rec) ->
    Rec#char_create_info{agility    = 0.6  * Rec#char_create_info.agility, 
                         intellect  = 0.7  * Rec#char_create_info.intellect,
                         spirit     = 0.77 * Rec#char_create_info.spirit,
                         stamina    = 1.2  * Rec#char_create_info.stamina, 
                         strength   = 1.1  * Rec#char_create_info.strength,
                         min_dmg    = 6,
                         max_dmg    = 7,
                         power_type = mana};
class_create_info(hunter, Rec) ->
    Rec#char_create_info{agility    = 1.3  * Rec#char_create_info.agility, 
                         intellect  = 0.6  * Rec#char_create_info.intellect,
                         spirit     = 0.66 * Rec#char_create_info.spirit,
                         stamina    = 0.89  * Rec#char_create_info.stamina, 
                         strength   = 0.49  * Rec#char_create_info.strength,
                         min_dmg    = 5,
                         max_dmg    = 7,
                         power_type = mana};
class_create_info(rogue, Rec) ->
    Rec#char_create_info{agility    = 1.5  * Rec#char_create_info.agility, 
                         intellect  = 0.22 * Rec#char_create_info.intellect,
                         spirit     = 0.42 * Rec#char_create_info.spirit,
                         stamina    = 0.78 * Rec#char_create_info.stamina, 
                         strength   = 0.83 * Rec#char_create_info.strength,
                         min_dmg    = 6,
                         max_dmg    = 7,
                         power_type = energy};
class_create_info(priest, Rec) ->
    Rec#char_create_info{agility    = 0.3  * Rec#char_create_info.agility, 
                         intellect  = 1.3  * Rec#char_create_info.intellect,
                         spirit     = 1.39 * Rec#char_create_info.spirit,
                         stamina    = 0.4  * Rec#char_create_info.stamina, 
                         strength   = 0.3  * Rec#char_create_info.strength,
                         min_dmg    = 2,
                         max_dmg    = 4,
                         power_type = mana};
class_create_info(shaman, Rec) ->
    Rec#char_create_info{agility    = 0.5  * Rec#char_create_info.agility, 
                         intellect  = 0.9  * Rec#char_create_info.intellect,
                         spirit     = 1    * Rec#char_create_info.spirit,
                         stamina    = 0.9  * Rec#char_create_info.stamina, 
                         strength   = 0.81 * Rec#char_create_info.strength,
                         min_dmg    = 2,
                         max_dmg    = 4,
                         power_type = mana};
class_create_info(mage, Rec) ->
    Rec#char_create_info{agility    = 0.2  * Rec#char_create_info.agility, 
                         intellect  = 1.44 * Rec#char_create_info.intellect,
                         spirit     = 1.33 * Rec#char_create_info.spirit,
                         stamina    = 0.33 * Rec#char_create_info.stamina, 
                         strength   = 0.14 * Rec#char_create_info.strength,
                         min_dmg    = 2,
                         max_dmg    = 4,
                         power_type = mana};
class_create_info(warlock, Rec) ->
    Rec#char_create_info{agility    = 0.4  * Rec#char_create_info.agility, 
                         intellect  = 1.2  * Rec#char_create_info.intellect,
                         spirit     = 0.7  * Rec#char_create_info.spirit,
                         stamina    = 0.7  * Rec#char_create_info.stamina, 
                         strength   = 0.37 * Rec#char_create_info.strength,
                         min_dmg    = 3.28,
                         max_dmg    = 5.28,
                         power_type = mana};
class_create_info(druid, Rec) ->
    Rec#char_create_info{agility    = 0.5  * Rec#char_create_info.agility, 
                         intellect  = 1    * Rec#char_create_info.intellect,
                         spirit     = 1.1  * Rec#char_create_info.spirit,
                         stamina    = 0.66 * Rec#char_create_info.stamina, 
                         strength   = 0.6  * Rec#char_create_info.strength,
                         min_dmg    = 2,
                         max_dmg    = 4,
                         power_type = mana}.


initial_spells(human, warrior) ->
	[78,81,107,196,198,201,203,204,522,668,2382,2457,2479,3050,3365,5301,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,8737,9077,9078,9116,9125,20597,20598,20599,20600,20864,21651,21652,22027,22810];
initial_spells(human, paladin) ->
	[81,107,198,199,203,204,522,635,668,2382,2479,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,8737,9077,9078,9116,9125,20597,20598,20599,20600,20864,21084,21651,21652,22027,22810,27762];
initial_spells(human, rogue) ->
	[81,203,204,522,668,1180,1752,2098,2382,2479,2567,2764,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,16092,20597,20598,20599,20600,20864,21184,21651,21652,22027,22810];
initial_spells(human, priest) ->
	[81,198,203,204,522,585,668,2050,2382,2479,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9078,9125,20597,20598,20599,20600,20864,21651,21652,22027,22810];
initial_spells(human, mage) ->
	[81,133,168,203,204,227,522,668,2382,2479,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9078,9125,20597,20598,20599,20600,20864,21651,21652,22027,22810];
initial_spells(human, warlock) ->
	[81,203,204,522,668,686,687,1180,2382,2479,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9078,9125,20597,20598,20599,20600,20864,21651,21652,22027,22810];

initial_spells(orc, warrior) ->
	[78,81,107,196,197,201,203,204,522,669,2382,2457,2479,3050,3365,5301,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,8737,9077,9078,9116,9125,20572,20573,20574,21563,21651,21652,22027,22810];
initial_spells(orc, hunter) ->
	[75,81,196,203,204,264,522,669,2382,2479,2973,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,13358,20572,20573,20574,20576,21651,21652,22027,22810,24949];
initial_spells(orc, rogue) ->
	[81,203,204,522,669,1180,1752,2098,2382,2479,2567,2764,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,16092,20572,20573,20574,21184,21563,21651,21652,22027,22810];
initial_spells(orc, shaman) ->
	[81,107,198,203,204,227,331,403,522,669,2382,2479,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9116,9125,20572,20573,20574,21563,21651,21652,22027,22810,27763];
initial_spells(orc, warlock) ->
[81,203,204,522,669,686,687,1180,2382,2479,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9078,9125,20572,20573,20574,20575,21651,21652,22027,22810];

initial_spells(dwarf, warrior) ->
	[78,81,107,196,197,198,203,204,522,668,672,2382,2457,2479,2481,3050,3365,5301,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,8737,9077,9078,9116,9125,20594,20595,20596,21651,21652,22027,22810];
initial_spells(dwarf, paladin) ->
	[81,107,198,199,203,204,522,635,668,672,2382,2479,2481,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,8737,9077,9078,9116,9125,20594,20595,20596,21084,21651,21652,22027,22810,27762];
initial_spells(dwarf, hunter) ->
	[75,81,196,203,204,266,522,668,672,2382,2479,2481,2973,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,13358,20594,20595,20596,21651,21652,22027,22810,24949];
initial_spells(dwarf, rogue) ->
	[81,203,204,522,668,672,1180,1752,2098,2382,2479,2481,2567,2764,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,16092,20594,20595,20596,21184,21651,21652,22027,22810];
initial_spells(dwarf, priest) ->
	[81,198,203,204,522,585,668,672,2050,2382,2479,2481,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9078,9125,20594,20595,20596,21651,21652,22027,22810];

initial_spells(night_elf, warrior) ->
	[78,81,107,198,201,203,204,522,668,671,1180,2382,2457,2479,3050,3365,5301,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,8737,9077,9078,9116,9125,20580,20582,20583,20585,21009,21651,21652,22027,22810];
initial_spells(night_elf, hunter) ->
	[75,81,203,204,264,522,668,671,1180,2382,2479,2973,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,13358,20580,20582,20583,20585,21009,21651,21652,22027,22810,24949];
initial_spells(night_elf, rogue) ->
	[81,203,204,522,668,671,1180,1752,2098,2382,2479,2567,2764,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,16092,20580,20582,20583,20585,21009,21184,21651,21652,22027,22810];
initial_spells(night_elf, priest) ->
	[81,198,203,204,522,585,668,671,2050,2382,2479,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9078,9125,20580,20582,20583,20585,21009,21651,21652,22027,22810];
initial_spells(night_elf, druid) ->
	[81,203,204,227,522,668,671,1180,2382,2479,3050,3365,5176,5185,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,20580,20582,20583,20585,21009,21651,21652,22027,22810,27764];

initial_spells(undead, warrior) ->
	[78,81,107,201,202,203,204,522,669,1180,2382,2457,2479,3050,3365,5227,5301,6233,6246,6247,6477,6478,6603,7266,7267,7355,7744,8386,8737,9077,9078,9116,9125,17737,20577,20579,21651,21652,22027,22810];
initial_spells(undead, rogue) ->
	[81,203,204,522,669,1180,1752,2098,2382,2479,2567,2764,3050,3365,5227,6233,6246,6247,6477,6478,6603,7266,7267,7355,7744,8386,9077,9078,9125,16092,17737,20577,20579,21184,21651,21652,22027,22810];
initial_spells(undead, priest) ->
	[81,198,203,204,522,585,669,2050,2382,2479,3050,3365,5009,5019,5227,6233,6246,6247,6477,6478,6603,7266,7267,7355,7744,8386,9078,9125,17737,20577,20579,21651,21652,22027,22810];
initial_spells(undead, mage) ->
	[81,133,168,203,204,227,522,669,2382,2479,3050,3365,5009,5019,5227,6233,6246,6247,6477,6478,6603,7266,7267,7355,7744,8386,9078,9125,17737,20577,20579,21651,21652,22027,22810];
initial_spells(undead, warlock) ->
	[81,203,204,522,669,686,687,1180,2382,2479,3050,3365,5009,5019,5227,6233,6246,6247,6477,6478,6603,7266,7267,7355,7744,8386,9078,9125,17737,20577,20579,21651,21652,22027,22810];

initial_spells(tauren, warrior) ->
	[78,81,107,196,198,199,203,204,522,669,670,2382,2457,2479,3050,3365,5301,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,8737,9077,9078,9116,9125,20549,20550,20551,20552,21651,21652,22027,22810];
initial_spells(tauren, hunter) ->
	[75,81,196,203,204,266,522,669,670,2382,2479,2973,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,13358,20549,20550,20551,20552,21651,21652,22027,22810,24949];
initial_spells(tauren, shaman) ->
	[81,107,198,203,204,227,331,403,522,669,670,2382,2479,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9116,9125,20549,20550,20551,20552,21651,21652,22027,22810,27763];
initial_spells(tauren, druid) ->
	[81,198,203,204,227,522,669,670,2382,2479,3050,3365,5176,5185,6233,6246,6247,6477,6478,6603,7266,7267,7355,8386,9077,9078,9125,20549,20550,20551,20552,21651,21652,22027,22810,27764];

initial_spells(gnome, warrior) ->
	[78,81,107,198,201,203,204,522,668,1180,2382,2457,2479,3050,3365,5301,6233,6246,6247,6477,6478,6603,7266,7267,7340,7355,8386,8737,9077,9078,9116,9125,20589,20591,20592,20593,21651,21652,22027,22810];
initial_spells(gnome, rogue) ->
	[81,203,204,522,668,1180,1752,2098,2382,2479,2567,2764,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7340,7355,8386,9077,9078,9125,16092,20589,20591,20592,20593,21184,21651,21652,22027,22810];
initial_spells(gnome, mage) ->
	[81,133,168,203,204,227,522,668,2382,2479,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7340,7355,8386,9078,9125,20589,20591,20592,20593,21651,21652,22027,22810];
initial_spells(gnome, warlock) ->
	[81,203,204,522,668,686,687,1180,2382,2479,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7340,7355,8386,9078,9125,20589,20591,20592,20593,21651,21652,22027,22810];

initial_spells(troll, warrior) ->
	[78,81,107,196,203,204,522,669,1180,2382,2457,2479,2567,2764,3050,3365,5301,6233,6246,6247,6477,6478,6603,7266,7267,7341,7355,8386,8737,9077,9078,9116,9125,20555,20557,20558,21651,21652,22027,22810,26290,26296];
initial_spells(troll, hunter) ->
	[75,81,196,203,204,264,522,669,2382,2479,2973,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7341,7355,8386,9077,9078,9125,13358,20554,20555,20557,20558,21651,21652,22027,22810,24949,26290];
initial_spells(troll, rogue) ->
	[81,203,204,522,669,1180,1752,2098,2382,2479,2567,2764,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7341,7355,8386,9077,9078,9125,16092,20555,20557,20558,21184,21651,21652,22027,22810,26290,26297];
initial_spells(troll, priest) ->
	[81,198,203,204,522,585,669,2050,2382,2479,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7341,7355,8386,9078,9125,20554,20555,20557,20558,21651,21652,22027,22810,26290];
initial_spells(troll, shaman) ->
	[81,107,198,203,204,227,331,403,522,669,2382,2479,3050,3365,6233,6246,6247,6477,6478,6603,7266,7267,7341,7355,8386,9077,9078,9116,9125,20554,20555,20557,20558,21651,21652,22027,22810,26290,27763];
initial_spells(troll, mage) ->
	[81,133,168,203,204,227,522,669,2382,2479,3050,3365,5009,5019,6233,6246,6247,6477,6478,6603,7266,7267,7341,7355,8386,9078,9125,20554,20555,20557,20558,21651,21652,22027,22810,26290].



initial_action_bars(human, warrior) ->
	[{0,6603,0}, {1,78,0}, {11,117,128}];
initial_action_bars(human, paladin) ->
	[{0,6603,0}, {1,21084,0}, {2,635,0}, {10,159,128}, {11,2070,128}];
initial_action_bars(human, rogue) ->
	[{0,6603,0}, {1,1752,0}, {2,2098,0}, {3,2764,0}, {11,2070,128}];
initial_action_bars(human, priest) ->
	[{0,6603,0}, {1,585,0}, {2,2050,0}, {10,159,128}, {11,2070,128}];
initial_action_bars(human, mage) ->
	[{0,6603,0}, {1,133,0}, {2,168,0}, {10,159,128}, {11,2070,128}];
initial_action_bars(human, warlock) ->
	[{0,6603,0}, {1,686,0}, {2,687,0}, {10,159,128}, {11,4604,128}];
initial_action_bars(orc, warrior) ->
	[{0,6603,0}, {1,78,0}, {2,20572,0}, {11,117,128}];
initial_action_bars(orc, hunter) ->
	[{0,6603,0}, {1,2973,0}, {2,75,0}, {3,20572,0}, {10,159,128}, {11,117,128}];
initial_action_bars(orc, rogue) ->
	[{0,6603,0}, {1,1752,0}, {2,2098,0}, {3,2764,0}, {4,20572,0}, {11,117,128}];
initial_action_bars(orc, shaman) ->
	[{0,6603,0}, {1,403,0}, {2,331,0}, {3,20572,0}, {10,159,128}, {11,117,128}];
initial_action_bars(orc, warlock) ->
	[{0,6603,0}, {1,686,0}, {2,687,0}, {3,20572,0}, {10,159,128}, {11,117,128}];
initial_action_bars(dwarf, warrior) ->
	[{0,6603,0}, {1,78,0}, {2,20594,0}, {3,2481,0}, {11,117,128}];
initial_action_bars(dwarf, paladin) ->
	[{0,6603,0}, {1,21084,0}, {2,635,0}, {3,20594,0}, {4,2481,0}, {10,159,128}, {11,4540,128}];
initial_action_bars(dwarf, hunter) ->
	[{0,6603,0}, {1,2973,0}, {2,75,0}, {3,20594,0}, {4,2481,0}, {10,159,128}, {11,117,128}];
initial_action_bars(dwarf, rogue) ->
	[{0,6603,0}, {1,1752,0}, {2,2098,0}, {3,2764,0}, {4,20594,0}, {5,2481,0}, {11,4540,128}];
initial_action_bars(dwarf, priest) ->
	[{0,6603,0}, {1,585,0}, {2,2050,0}, {3,20594,0}, {4,2481,0}, {10,159,128}, {11,4540,128}];
initial_action_bars(night_elf, warrior) ->
	[{0,6603,0}, {1,78,0}, {2,20580,0}, {11,117,128}];
initial_action_bars(night_elf, hunter) ->
	[{0,6603,0}, {1,2973,0}, {2,75,0}, {3,20580,0}, {10,159,128}, {11,117,128}];
initial_action_bars(night_elf, rogue) ->
	[{0,6603,0}, {1,1752,0}, {2,2098,0}, {3,2764,0}, {4,20580,0}, {11,4540,128}];
initial_action_bars(night_elf, priest) ->
	[{0,6603,0}, {1,585,0}, {2,2050,0}, {3,20580,0}, {10,159,128}, {11,2070,128}];
initial_action_bars(night_elf, druid) ->
	[{0,6603,0}, {1,5176,0}, {2,5185,0}, {3,20580,0}, {10,159,128}, {11,4536,128}];
initial_action_bars(undead, warrior) ->
	[{0,6603,0}, {1,78,0}, {2,20577,0}, {11,4604,128}];
initial_action_bars(undead, rogue) ->
	[{0,6603,0}, {1,1752,0}, {2,2098,0}, {3,2764,0}, {4,20577,0}, {11,4604,128}];
initial_action_bars(undead, priest) ->
	[{0,6603,0}, {1,585,0}, {2,2050,0}, {3,20577,0}, {10,159,128}, {11,4604,128}];
initial_action_bars(undead, mage) ->
	[{0,6603,0}, {1,133,0}, {2,168,0}, {3,20577,0}, {10,159,128}, {11,4604,128}];
initial_action_bars(undead, warlock) ->
	[{0,6603,0}, {1,686,0}, {2,687,0}, {3,20577,0}, {10,159,128}, {11,4604,128}];
initial_action_bars(tauren, warrior) ->
	[{0,6603,0}, {1,78,0}, {2,20549,0}, {11,4540,128}];
initial_action_bars(tauren, hunter) ->
	[{0,6603,0}, {1,2973,0}, {2,75,0}, {3,20549,0}, {10,159,128}, {11,117,128}];
initial_action_bars(tauren, shaman) ->
	[{0,6603,0}, {1,403,0}, {2,331,0}, {3,20549,0}, {10,159,128}, {11,4604,128}];
initial_action_bars(tauren, druid) ->
	[{0,6603,0}, {1,5176,0}, {2,5185,0}, {3,20549,0}, {10,159,128}, {11,4536,128}];
initial_action_bars(gnome, warrior) ->
	[{0,6603,0}, {1,78,0}, {11,117,128}];
initial_action_bars(gnome, rogue) ->
	[{0,6603,0}, {1,1752,0}, {2,2098,0}, {3,2764,0}, {11,117,128}];
initial_action_bars(gnome, mage) ->
	[{0,6603,0}, {1,133,0}, {2,168,0}, {10,159,128}, {11,4536,128}];
initial_action_bars(gnome, warlock) ->
	[{0,6603,0}, {1,686,0}, {2,687,0}, {10,159,128}, {11,4604,128}];
initial_action_bars(troll, warrior) ->
	[{0,6603,0}, {1,78,0}, {2,2764,0}, {3,26296,0}, {11,117,128}];
initial_action_bars(troll, hunter) ->
	[{0,6603,0}, {1,2973,0}, {2,75,0}, {3,20554,0}, {10,159,128}, {11,4604,128}];
initial_action_bars(troll, rogue) ->
	[{0,6603,0}, {1,1752,0}, {2,2098,0}, {3,2764,0}, {4,26297,0}, {11,117,128}];
initial_action_bars(troll, priest) ->
	[{0,6603,0}, {1,585,0}, {2,2050,0}, {3,20554,0}, {10,159,128}, {11,4540,128}];
initial_action_bars(troll, shaman) ->
	[{0,6603,0}, {1,403,0}, {2,331,0}, {3,20554,0}, {10,159,128}, {11,117,128}];
initial_action_bars(troll, mage) ->
	[{0,6603,0}, {1,133,0}, {2,168,0}, {3,20554,0}, {10,159,128}, {11,117,128}].
