-module(content).
-export([char_create_info/2]).
-export([lookup_item/1]).

-include("include/database_records.hrl").

%% @type race() = human | orc | dwarf | night_elf |
%%                undead | tauren | gnome | troll.
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
race_class_create_info(human,     warrior, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(orc,       warrior, Rec) -> Rec#char_create_info{health = 80, power = 1000};
race_class_create_info(dwarf,     warrior, Rec) -> Rec#char_create_info{health = 90, power = 1000};
race_class_create_info(night_elf, warrior, Rec) -> Rec#char_create_info{health = 50, power = 1000};
race_class_create_info(undead,    warrior, Rec) -> Rec#char_create_info{health = 70, power = 1000};
race_class_create_info(tauren,    warrior, Rec) -> Rec#char_create_info{health = 80, power = 1000};
race_class_create_info(gnome,     warrior, Rec) -> Rec#char_create_info{health = 50, power = 1000};
race_class_create_info(troll,     warrior, Rec) -> Rec#char_create_info{health = 70, power = 1000};

race_class_create_info(human,     paladin, Rec) -> Rec#char_create_info{health = 58, power = 80};
race_class_create_info(dwarf,     paladin, Rec) -> Rec#char_create_info{health = 88, power = 79};

race_class_create_info(orc,       hunter, Rec) -> Rec#char_create_info{health = 76, power = 82};
race_class_create_info(dwarf,     hunter, Rec) -> Rec#char_create_info{health = 86, power = 84};
race_class_create_info(night_elf, hunter, Rec) -> Rec#char_create_info{health = 46, power = 85};
race_class_create_info(tauren,    hunter, Rec) -> Rec#char_create_info{health = 76, power = 80};
race_class_create_info(troll,     hunter, Rec) -> Rec#char_create_info{health = 66, power = 81};

race_class_create_info(human,     rogue, Rec) -> Rec#char_create_info{health = 55, power = 100};
race_class_create_info(orc,       rogue, Rec) -> Rec#char_create_info{health = 75, power = 100};
race_class_create_info(dwarf,     rogue, Rec) -> Rec#char_create_info{health = 85, power = 100};
race_class_create_info(night_elf, rogue, Rec) -> Rec#char_create_info{health = 45, power = 100};
race_class_create_info(undead,    rogue, Rec) -> Rec#char_create_info{health = 65, power = 100};
race_class_create_info(gnome,     rogue, Rec) -> Rec#char_create_info{health = 45, power = 100};
race_class_create_info(troll,     rogue, Rec) -> Rec#char_create_info{health = 65, power = 100};

race_class_create_info(human,     priest, Rec) -> Rec#char_create_info{health = 52, power = 160};
race_class_create_info(dwarf,     priest, Rec) -> Rec#char_create_info{health = 82, power = 145};
race_class_create_info(night_elf, priest, Rec) -> Rec#char_create_info{health = 51, power = 160};
race_class_create_info(undead,    priest, Rec) -> Rec#char_create_info{health = 62, power = 130};
race_class_create_info(troll,     priest, Rec) -> Rec#char_create_info{health = 62, power = 128};

% TODO: find death knight start values
race_class_create_info(human,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(orc,       death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(dwarf,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(night_elf, death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(undead,    death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(tauren,    death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(gnome,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(troll,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};

race_class_create_info(orc,       shaman, Rec) -> Rec#char_create_info{health = 77, power = 73};
race_class_create_info(tauren,    shaman, Rec) -> Rec#char_create_info{health = 77, power = 71};
race_class_create_info(troll,     shaman, Rec) -> Rec#char_create_info{health = 67, power = 72};

race_class_create_info(human,     mage, Rec) -> Rec#char_create_info{health = 52, power = 165};
race_class_create_info(undead,    mage, Rec) -> Rec#char_create_info{health = 62, power = 135};
race_class_create_info(gnome,     mage, Rec) -> Rec#char_create_info{health = 51, power = 210};
race_class_create_info(troll,     mage, Rec) -> Rec#char_create_info{health = 62, power = 119};

race_class_create_info(human,     warlock, Rec) -> Rec#char_create_info{health = 53, power = 140};
race_class_create_info(orc,       warlock, Rec) -> Rec#char_create_info{health = 73, power = 109};
race_class_create_info(undead,    warlock, Rec) -> Rec#char_create_info{health = 63, power = 110};
race_class_create_info(gnome,     warlock, Rec) -> Rec#char_create_info{health = 43, power = 185};

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
    Rec#char_create_info{faction_template = 1, 
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
    Rec#char_create_info{faction_template = 1, 
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
    Rec#char_create_info{faction_template = 2, 
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
    Rec#char_create_info{faction_template = 2, 
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
    Rec#char_create_info{faction_template = 1, 
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
    Rec#char_create_info{faction_template = 2, 
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
	





lookup_item(ItemId) ->
	Items = get_item_list(),
	ItemProto = lists:keyfind(ItemId, 1, Items),
	if ItemProto /= false ->
			Class = element(2, ItemProto),
			SubClass = element(3, ItemProto),
			DisplayInfoId = element(5, ItemProto),
			InventoryType = element(11, ItemProto),
			Stackable = element(24, ItemProto),
			MaxDurability = element(118, ItemProto),
			#item_proto{id=ItemId, class=Class, sub_class=SubClass, display_info_id=DisplayInfoId, inventory_type=InventoryType, stackable=Stackable, max_durability=MaxDurability};
		ItemProto == false -> false
	end.


get_item_list() ->
	[
		{25,2,7,<<"Worn Shortsword">>,1542,1,0,1,35,7,21,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1900,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,1,3,0,0,0,20,0,0,0,<<"">>,0,0,0,0,0,0},
		{35,2,10,<<"Bent Staff">>,472,1,0,1,47,9,17,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2900,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,2,2,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{36,2,4,<<"Worn Mace">>,5194,1,0,1,38,7,21,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1900,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,2,3,0,0,0,20,0,0,0,<<"">>,0,0,0,0,0,0},
		{37,2,0,<<"Worn Axe">>,14029,1,0,1,38,7,21,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,1,3,0,0,0,20,0,0,0,<<"">>,0,0,0,0,0,0},
		{38,4,0,<<"Recruis Shirt">>,9891,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{39,4,1,<<"Recruit\'s Pants">>,9892,0,0,1,5,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{40,4,0,<<"Recruit\'s Boots">>,10141,1,0,1,4,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{43,4,0,<<"Squire\'s Boots">>,10272,1,0,1,4,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{44,4,1,<<"Squire\'s Pants">>,9937,0,0,1,4,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{45,4,0,<<"Squire\'s Shirt">>,3265,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{47,4,0,<<"Footpad\'s Shoes">>,9915,1,0,1,4,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{48,4,1,<<"Footpad\'s Pants">>,9913,0,0,1,4,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{49,4,0,<<"Footpad\'s Shirt">>,9906,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{51,4,0,<<"Neophyte\'s Boots">>,9946,1,0,1,5,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{52,4,1,<<"Neophyte\'s Pants">>,9945,0,0,1,5,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{53,4,0,<<"Neophyte\'s Shirt">>,9944,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{55,4,0,<<"Apprentice\'s Boots">>,9929,1,0,1,5,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{56,4,1,<<"Apprentice\'s Robe">>,12647,0,0,1,5,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,35,0,0,0,<<"">>,0,0,0,0,0,0},
		{57,4,1,<<"Acolyte\'s Robe">>,12645,0,0,1,5,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,35,0,0,0,<<"">>,0,0,0,0,0,0},
		{59,4,0,<<"Acolyte\'s Shoes">>,3261,1,0,1,5,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{117,0,0,<<"Tough Jerky">>,2473,1,0,5,25,1,0,-1,-1,5,1,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,433,0,-1,0,0,11,1000,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,1,0,0,0,0},
		{120,4,1,<<"Thug Pants">>,10006,0,0,1,4,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{121,4,0,<<"Thug Boots">>,10008,1,0,1,4,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{127,4,0,<<"Trapper\'s Shirt">>,9996,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{129,4,0,<<"Rugged Trapper\'s Boots">>,9977,1,0,1,5,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{139,4,1,<<"Brawler\'s Pants">>,9988,0,0,1,4,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{140,4,0,<<"Brawler\'s Boots">>,9992,1,0,1,4,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{147,4,1,<<"Rugged Trapper\'s Pants">>,9975,0,0,1,5,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{148,4,0,<<"Rugged Trapper\'s Shirt">>,9976,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{153,4,2,<<"Primitive Kilt">>,10050,0,0,1,5,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,8,0,0,0,0,30,0,0,0,<<"">>,0,0,0,0,0,0},
		{154,4,0,<<"Primitive Mantle">>,10058,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{159,0,0,<<"Refreshing Spring Water">>,18084,1,0,5,25,1,0,-1,-1,5,1,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,430,0,-1,0,0,59,1000,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{1395,4,1,<<"Apprentice\'s Pants">>,9924,0,0,1,5,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{1396,4,1,<<"Acolyte\'s Pants">>,3260,0,0,1,4,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{2070,0,0,<<"Darnassian Bleu">>,6353,1,0,5,25,1,0,-1,-1,5,1,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,433,0,-1,0,0,11,1000,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,3,0,0,0,0},
		{2092,2,15,<<"Worn Dagger">>,6442,1,0,1,35,7,13,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1600,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,1,3,0,0,0,16,0,0,0,<<"">>,0,0,0,0,0,0},
		{2101,11,2,<<"Light Quiver">>,21328,1,0,1,4,1,18,-1,-1,1,1,0,0,0,0,0,0,0,0,1,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,14824,1,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{2102,11,3,<<"Small Ammo Pouch">>,1816,1,0,1,4,1,18,-1,-1,1,1,0,0,0,0,0,0,0,0,1,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,14824,1,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{2105,4,0,<<"Thug Shirt">>,10005,1,0,1,5,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{2361,2,5,<<"Battleworn Hammer">>,8690,1,0,1,45,9,17,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2900,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,2,1,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{2362,4,6,<<"Worn Wooden Shield">>,18730,0,0,1,7,1,14,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,1,4,0,1,0,20,0,0,0,<<"">>,0,0,0,0,0,0},
		{2504,2,2,<<"Worn Shortbow">>,8106,1,0,1,29,5,15,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2300,2,100,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,2,0,0,0,0,20,0,0,0,<<"">>,0,0,0,0,0,0},
		{2508,2,3,<<"Old Blunderbuss">>,6606,1,0,1,27,5,26,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2300,3,100,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,1,0,0,0,0,20,0,0,0,<<"">>,0,0,0,0,0,0},
		{2512,6,2,<<"Rough Arrow">>,5996,1,0,200,10,0,24,-1,-1,5,1,0,0,0,0,0,0,0,0,200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,2,0,0,0,0,0,0,0,1,<<"">>,0,0,0,0,0,0},
		{2516,6,3,<<"Light Shot">>,5998,1,0,200,10,0,24,-1,-1,5,1,0,0,0,0,0,0,0,0,200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,2,0,0,0,0,0,0,0,2,<<"">>,0,0,0,0,0,0},
		{2947,2,16,<<"Small Throwing Knife">>,16754,1,0,200,15,0,25,-1,-1,3,1,0,0,0,0,0,0,0,0,200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2000,4,100,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,1,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{3111,2,16,<<"Crude Throwing Axe">>,20777,1,0,200,15,0,25,-1,-1,3,1,0,0,0,0,0,0,0,0,200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2000,4,100,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,1,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{3661,2,10,<<"Handcrafted Staff">>,18530,1,0,1,45,9,17,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2900,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,2,2,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{4536,0,0,<<"Shiny Red Apple">>,6410,1,0,5,25,1,0,-1,-1,5,1,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,433,0,-1,0,0,11,1000,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,6,0,0,0,0},
		{4540,0,0,<<"Tough Hunk of Bread">>,6399,1,0,5,25,1,0,-1,-1,5,1,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,433,0,-1,0,0,11,1000,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,4,0,0,0,0},
		{4604,0,0,<<"Forest Mushroom Cap">>,15852,1,0,5,25,1,0,-1,-1,5,1,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,433,0,-1,0,0,11,1000,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,5,0,0,0,0},
		{6096,4,0,<<"Apprentice\'s Shirt">>,2163,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6097,4,0,<<"Acolyte\'s Shirt">>,2470,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6098,4,1,<<"Neophyte\'s Robe">>,12679,0,0,1,4,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,35,0,0,0,<<"">>,0,0,0,0,0,0},
		{6116,4,1,<<"Apprentice\'s Robe">>,12648,0,0,1,4,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6117,4,0,<<"Squire\'s Shirt">>,9972,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6118,4,1,<<"Squire\'s Pants">>,9974,0,0,1,4,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{6119,4,1,<<"Neophyte\'s Robe">>,12681,0,0,1,4,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,35,0,0,0,<<"">>,0,0,0,0,0,0},
		{6120,4,0,<<"Recruit\'s Shirt">>,9983,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6121,4,1,<<"Recruit\'s Pants">>,9984,0,0,1,4,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{6122,4,0,<<"Recruit\'s Boots">>,9985,1,0,1,4,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6123,4,1,<<"Novice\'s Robe">>,12683,0,0,1,4,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,35,0,0,0,<<"">>,0,0,0,0,0,0},
		{6124,4,1,<<"Novice\'s Pants">>,9987,0,0,1,5,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{6125,4,0,<<"Brawler\'s Harness">>,9995,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6126,4,1,<<"Trapper\'s Pants">>,10002,0,0,1,5,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{6127,4,0,<<"Trapper\'s Boots">>,10003,1,0,1,5,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6129,4,1,<<"Acolyte\'s Robe">>,12646,0,0,1,5,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,35,0,0,0,<<"">>,0,0,0,0,0,0},
		{6134,4,0,<<"Primitive Mantle">>,10108,1,0,1,1,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6135,4,2,<<"Primitive Kilt">>,10109,0,0,1,5,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,8,0,0,0,0,30,0,0,0,<<"">>,0,0,0,0,0,0},
		{6136,4,0,<<"Thug Shirt">>,10112,1,0,1,4,1,4,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6137,4,1,<<"Thug Pants">>,10114,0,0,1,4,1,7,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{6138,4,0,<<"Thug Boots">>,10115,1,0,1,4,1,8,-1,-1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{6139,4,1,<<"Novice\'s Robe">>,12684,0,0,1,4,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,35,0,0,0,<<"">>,0,0,0,0,0,0},
		{6140,4,1,<<"Apprentice\'s Robe">>,12649,0,0,1,4,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,35,0,0,0,<<"">>,0,0,0,0,0,0},
		{6144,4,1,<<"Neophyte\'s Robe">>,12680,0,0,1,5,1,20,-1,-1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,7,0,0,0,0,35,0,0,0,<<"">>,0,0,0,0,0,0},
		{6948,15,0,<<"Hearthstone">>,6418,1,64,1,0,0,0,-1,-1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,8690,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,1,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{8164,0,0,<<"Test Stationery">>,1069,1,0,1,10,2,0,-1,-1,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{9311,0,0,<<"Default Stationery">>,7798,1,0,1,0,0,0,-1,-1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,1,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{12282,2,1,<<"Worn Battleaxe">>,22291,1,0,1,43,8,17,-1,-1,2,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2900,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,1,1,0,0,0,25,0,0,0,<<"">>,0,0,0,0,0,0},
		{18154,0,0,<<"Blizzard Stationery">>,30658,1,0,1,0,0,0,-1,-1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{21140,0,0,<<"Auction Stationery">>,1102,1,0,1,0,0,0,-1,-1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,0,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0},
		{22058,0,0,<<"Valentine\'s Day Stationery">>,34508,1,0,1,0,0,0,-1,-1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,0,0,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,0,0,0,-1,0,-1,0,<<"">>,0,0,6,0,0,0,0,0,0,0,0,0,0,0,<<"">>,0,0,0,0,0,0}
	].

