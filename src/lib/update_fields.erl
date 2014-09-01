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

-module(update_fields).

-export([field_data/1, get_total_count/1]).
-export([type/1, fields/1]).


fields(Field) ->
	{Index, _} = field_data(Field),
	Index.

type(Field) ->
	{_, Type} = field_data(Field),
	Type.
	


get_total_count(player) ->
	update_fields:fields('PLAYER_END');
get_total_count(object) ->
	update_fields:fields('OBJECT_END');
get_total_count(item) ->
	update_fields:fields('ITEM_END');
get_total_count(container) ->
	update_fields:fields('CONTAINER_END');
get_total_count(unit) ->
	update_fields:fields('UNIT_END');
get_total_count(game_object) ->
	update_fields:fields('GAMEOBJECT_END');
get_total_count(dynamic_object) ->
	update_fields:fields('DYNAMICOBJECT_END');
get_total_count(corpse) ->
	update_fields:fields('CORPSE_END').



field_data('OBJECT_FIELD_GUID') -> {16#00, uint64};
field_data('OBJECT_FIELD_TYPE') -> {16#02, uint32};
field_data('OBJECT_FIELD_ENTRY') -> {16#03, uint32};
field_data('OBJECT_FIELD_SCALE_X') -> {16#04, float};
field_data('OBJECT_FIELD_PADDING') -> {16#05, uint32};
field_data('OBJECT_END') -> {16#06, uint32};

field_data('ITEM_FIELD_OWNER') -> {fields('OBJECT_END') + 16#00, uint64};
field_data('ITEM_FIELD_CONTAINED') -> {fields('OBJECT_END') + 16#02, uint64};
field_data('ITEM_FIELD_CREATOR') -> {fields('OBJECT_END') + 16#04, uint64};
field_data('ITEM_FIELD_GIFTCREATOR') -> {fields('OBJECT_END') + 16#06, uint64};
field_data('ITEM_FIELD_STACK_COUNT') -> {fields('OBJECT_END') + 16#08, uint32};
field_data('ITEM_FIELD_DURATION') -> {fields('OBJECT_END') + 16#09, uint32};
field_data('ITEM_FIELD_SPELL_CHARGES') -> {fields('OBJECT_END') + 16#0A, int32};
field_data('ITEM_FIELD_SPELL_CHARGES_01') -> {fields('OBJECT_END') + 16#0B, int32};
field_data('ITEM_FIELD_SPELL_CHARGES_02') -> {fields('OBJECT_END') + 16#0C, int32};
field_data('ITEM_FIELD_SPELL_CHARGES_03') -> {fields('OBJECT_END') + 16#0D, int32};
field_data('ITEM_FIELD_SPELL_CHARGES_04') -> {fields('OBJECT_END') + 16#0E, int32};
field_data('ITEM_FIELD_FLAGS') -> {fields('OBJECT_END') + 16#0F, uint32};
field_data('ITEM_FIELD_ENCHANTMENT') -> {fields('OBJECT_END') + 16#10, uint32};
field_data('ITEM_FIELD_PROPERTY_SEED') -> {fields('OBJECT_END') + 16#25, uint32};
field_data('ITEM_FIELD_RANDOM_PROPERTIES_ID') -> {fields('OBJECT_END') + 16#26, int32};
field_data('ITEM_FIELD_ITEM_TEXT_ID') -> {fields('OBJECT_END') + 16#27, uint32};
field_data('ITEM_FIELD_DURABILITY') -> {fields('OBJECT_END') + 16#28, uint32};
field_data('ITEM_FIELD_MAXDURABILITY') -> {fields('OBJECT_END') + 16#29, uint32};
field_data('ITEM_END') -> {fields('OBJECT_END') + 16#2A, uint32};

field_data('CONTAINER_FIELD_NUM_SLOTS') -> {fields('ITEM_END') + 16#00, uint32};
field_data('CONTAINER_ALIGN_PAD') -> {fields('ITEM_END') + 16#01, uint32};
field_data('CONTAINER_FIELD_SLOT_1') -> {fields('ITEM_END') + 16#02, uint64};
field_data('CONTAINER_FIELD_SLOT_LAST') -> {fields('ITEM_END') + 16#38, uint32};
field_data('CONTAINER_END') -> {fields('ITEM_END') + 16#3A, uint32};

field_data('UNIT_FIELD_CHARM') -> {16#00 + fields('OBJECT_END'), uint64};
field_data('UNIT_FIELD_SUMMON') -> {16#02 + fields('OBJECT_END'), uint64};
field_data('UNIT_FIELD_CHARMEDBY') -> {16#04 + fields('OBJECT_END'), uint64};
field_data('UNIT_FIELD_SUMMONEDBY') -> {16#06 + fields('OBJECT_END'), uint64};
field_data('UNIT_FIELD_CREATEDBY') -> {16#08 + fields('OBJECT_END'), uint64};
field_data('UNIT_FIELD_TARGET') -> {16#0A + fields('OBJECT_END'), uint64};
field_data('UNIT_FIELD_PERSUADED') -> {16#0C + fields('OBJECT_END'), uint64};
field_data('UNIT_FIELD_CHANNEL_OBJECT') -> {16#0E + fields('OBJECT_END'), uint64};
field_data('UNIT_FIELD_HEALTH') -> {16#10 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_POWER1') -> {16#11 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_POWER2') -> {16#12 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_POWER3') -> {16#13 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_POWER4') -> {16#14 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_POWER5') -> {16#15 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_MAXHEALTH') -> {16#16 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_MAXPOWER1') -> {16#17 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_MAXPOWER2') -> {16#18 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_MAXPOWER3') -> {16#19 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_MAXPOWER4') -> {16#1A + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_MAXPOWER5') -> {16#1B + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_LEVEL') -> {16#1C + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_FACTIONTEMPLATE') -> {16#1D + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_BYTES_0') -> {16#1E + fields('OBJECT_END'), uint8};
field_data('UNIT_VIRTUAL_ITEM_SLOT_DISPLAY') -> {16#1F + fields('OBJECT_END'), uint32};
field_data('UNIT_VIRTUAL_ITEM_SLOT_DISPLAY_01') -> {16#20 + fields('OBJECT_END'), uint32};
field_data('UNIT_VIRTUAL_ITEM_SLOT_DISPLAY_02') -> {16#21 + fields('OBJECT_END'), uint32};
field_data('UNIT_VIRTUAL_ITEM_INFO') -> {16#22 + fields('OBJECT_END'), uint32};
field_data('UNIT_VIRTUAL_ITEM_INFO_01') -> {16#23 + fields('OBJECT_END'), uint32};
field_data('UNIT_VIRTUAL_ITEM_INFO_02') -> {16#24 + fields('OBJECT_END'), uint32};
field_data('UNIT_VIRTUAL_ITEM_INFO_03') -> {16#25 + fields('OBJECT_END'), uint32};
field_data('UNIT_VIRTUAL_ITEM_INFO_04') -> {16#26 + fields('OBJECT_END'), uint32};
field_data('UNIT_VIRTUAL_ITEM_INFO_05') -> {16#27 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_FLAGS') -> {16#28 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURA') -> {16#29 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURA_LAST') -> {16#58 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURAFLAGS') -> {16#59 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURAFLAGS_01') -> {16#5a + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURAFLAGS_02') -> {16#5b + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURAFLAGS_03') -> {16#5c + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURAFLAGS_04') -> {16#5d + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURAFLAGS_05') -> {16#5e + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURALEVELS') -> {16#5f + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURALEVELS_LAST') -> {16#6a + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURAAPPLICATIONS') -> {16#6b + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURAAPPLICATIONS_LAST') -> {16#76 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_AURASTATE') -> {16#77 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_BASEATTACKTIME') -> {16#78 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_OFFHANDATTACKTIME') -> {16#79 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_RANGEDATTACKTIME') -> {16#7a + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_BOUNDINGRADIUS') -> {16#7b + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_COMBATREACH') -> {16#7c + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_DISPLAYID') -> {16#7d + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_NATIVEDISPLAYID') -> {16#7e + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_MOUNTDISPLAYID') -> {16#7f + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_MINDAMAGE') -> {16#80 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_MAXDAMAGE') -> {16#81 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_MINOFFHANDDAMAGE') -> {16#82 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_MAXOFFHANDDAMAGE') -> {16#83 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_BYTES_1') -> {16#84 + fields('OBJECT_END'), uint8};
field_data('UNIT_FIELD_PETNUMBER') -> {16#85 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_PET_NAME_TIMESTAMP') -> {16#86 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_PETEXPERIENCE') -> {16#87 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_PETNEXTLEVELEXP') -> {16#88 + fields('OBJECT_END'), uint32};
field_data('UNIT_DYNAMIC_FLAGS') -> {16#89 + fields('OBJECT_END'), uint32};
field_data('UNIT_CHANNEL_SPELL') -> {16#8a + fields('OBJECT_END'), uint32};
field_data('UNIT_MOD_CAST_SPEED') -> {16#8b + fields('OBJECT_END'), uint32};
field_data('UNIT_CREATED_BY_SPELL') -> {16#8c + fields('OBJECT_END'), uint32};
field_data('UNIT_NPC_FLAGS') -> {16#8d + fields('OBJECT_END'), uint32};
field_data('UNIT_NPC_EMOTESTATE') -> {16#8e + fields('OBJECT_END'), uint32};
field_data('UNIT_TRAINING_POINTS') -> {16#8f + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_STAT0') -> {16#90 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_STAT1') -> {16#91 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_STAT2') -> {16#92 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_STAT3') -> {16#93 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_STAT4') -> {16#94 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_RESISTANCES') -> {16#95 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_RESISTANCES_01') -> {16#96 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_RESISTANCES_02') -> {16#97 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_RESISTANCES_03') -> {16#98 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_RESISTANCES_04') -> {16#99 + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_RESISTANCES_05') -> {16#9a + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_RESISTANCES_06') -> {16#9b + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_BASE_MANA') -> {16#9c + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_BASE_HEALTH') -> {16#9d + fields('OBJECT_END'), uint32};
field_data('UNIT_FIELD_BYTES_2') -> {16#9e + fields('OBJECT_END'), uint8};
field_data('UNIT_FIELD_ATTACK_POWER') -> {16#9f + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_ATTACK_POWER_MODS') -> {16#a0 + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_ATTACK_POWER_MULTIPLIER') -> {16#a1 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_RANGED_ATTACK_POWER') -> {16#a2 + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_RANGED_ATTACK_POWER_MODS') -> {16#a3 + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_RANGED_ATTACK_POWER_MULTIPLIER') -> {16#a4 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_MINRANGEDDAMAGE') -> {16#a5 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_MAXRANGEDDAMAGE') -> {16#a6 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_POWER_COST_MODIFIER') -> {16#a7 + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_POWER_COST_MODIFIER_01') -> {16#a8 + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_POWER_COST_MODIFIER_02') -> {16#a9 + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_POWER_COST_MODIFIER_03') -> {16#aa + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_POWER_COST_MODIFIER_04') -> {16#ab + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_POWER_COST_MODIFIER_05') -> {16#ac + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_POWER_COST_MODIFIER_06') -> {16#ad + fields('OBJECT_END'), int32};
field_data('UNIT_FIELD_POWER_COST_MULTIPLIER') -> {16#ae + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_POWER_COST_MULTIPLIER_01') -> {16#af + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_POWER_COST_MULTIPLIER_02') -> {16#b0 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_POWER_COST_MULTIPLIER_03') -> {16#b1 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_POWER_COST_MULTIPLIER_04') -> {16#b2 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_POWER_COST_MULTIPLIER_05') -> {16#b3 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_POWER_COST_MULTIPLIER_06') -> {16#b4 + fields('OBJECT_END'), float};
field_data('UNIT_FIELD_PADDING') -> {16#b5 + fields('OBJECT_END'), uint32};
field_data('UNIT_END') -> {16#b6 + fields('OBJECT_END'), uint32};

field_data('PLAYER_DUEL_ARBITER') -> {16#00 + fields('UNIT_END'), uint64};
field_data('PLAYER_FLAGS') -> {16#02 + fields('UNIT_END'), uint32};
field_data('PLAYER_GUILDID') -> {16#03 + fields('UNIT_END'), uint32};
field_data('PLAYER_GUILDRANK') -> {16#04 + fields('UNIT_END'), uint32};
field_data('PLAYER_BYTES') -> {16#05 + fields('UNIT_END'), uint8};
field_data('PLAYER_BYTES_2') -> {16#06 + fields('UNIT_END'), uint8};
field_data('PLAYER_BYTES_3') -> {16#07 + fields('UNIT_END'), uint8};
field_data('PLAYER_DUEL_TEAM') -> {16#08 + fields('UNIT_END'), uint32};
field_data('PLAYER_GUILD_TIMESTAMP') -> {16#09 + fields('UNIT_END'), uint32};
field_data('PLAYER_QUEST_LOG_1_1') -> {16#0A + fields('UNIT_END'), uint32};
field_data('PLAYER_QUEST_LOG_1_2') -> {16#0B + fields('UNIT_END'), uint32};
field_data('PLAYER_QUEST_LOG_1_3') -> {16#0C + fields('UNIT_END'), uint32};
field_data('PLAYER_QUEST_LOG_LAST_1') -> {16#43 + fields('UNIT_END'), uint32};
field_data('PLAYER_QUEST_LOG_LAST_2') -> {16#44 + fields('UNIT_END'), uint32};
field_data('PLAYER_QUEST_LOG_LAST_3') -> {16#45 + fields('UNIT_END'), uint32};
field_data('PLAYER_VISIBLE_ITEM_1_CREATOR') -> {16#46 + fields('UNIT_END'), uint64};
field_data('PLAYER_VISIBLE_ITEM_1_0') -> {16#48 + fields('UNIT_END'), uint32};
field_data('PLAYER_VISIBLE_ITEM_1_PROPERTIES') -> {16#50 + fields('UNIT_END'), uint32};
field_data('PLAYER_VISIBLE_ITEM_1_PAD') -> {16#51 + fields('UNIT_END'), uint32};
field_data('PLAYER_VISIBLE_ITEM_LAST_CREATOR') -> {16#11e + fields('UNIT_END'), uint32};
field_data('PLAYER_VISIBLE_ITEM_LAST_0') -> {16#120 + fields('UNIT_END'), uint32};
field_data('PLAYER_VISIBLE_ITEM_LAST_PROPERTIES') -> {16#128 + fields('UNIT_END'), uint32};
field_data('PLAYER_VISIBLE_ITEM_LAST_PAD') -> {16#129 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_INV_SLOT_HEAD') -> {16#12a + fields('UNIT_END'), uint64};
field_data('PLAYER_FIELD_PACK_SLOT_1') -> {16#158 + fields('UNIT_END'), uint64};
field_data('PLAYER_FIELD_PACK_SLOT_LAST') -> {16#176 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_BANK_SLOT_1') -> {16#178 + fields('UNIT_END'), uint64};
field_data('PLAYER_FIELD_BANK_SLOT_LAST') -> {16#1a6 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_BANKBAG_SLOT_1') -> {16#1a8 + fields('UNIT_END'), uint64};
field_data('PLAYER_FIELD_BANKBAG_SLOT_LAST') -> {16#ab2 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_VENDORBUYBACK_SLOT_1') -> {16#1b4 + fields('UNIT_END'), uint64};
field_data('PLAYER_FIELD_VENDORBUYBACK_SLOT_LAST') -> {16#1ca + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_KEYRING_SLOT_1') -> {16#1cc + fields('UNIT_END'), uint64};
field_data('PLAYER_FIELD_KEYRING_SLOT_LAST') -> {16#20a + fields('UNIT_END'), uint32};
field_data('PLAYER_FARSIGHT') -> {16#20c + fields('UNIT_END'), uint64};
field_data('PLAYER_FIELD_COMBO_TARGET') -> {16#20e + fields('UNIT_END'), uint64};
field_data('PLAYER_XP') -> {16#210 + fields('UNIT_END'), uint32};
field_data('PLAYER_NEXT_LEVEL_XP') -> {16#211 + fields('UNIT_END'), uint32};
field_data('PLAYER_SKILL_INFO_1_1') -> {16#212 + fields('UNIT_END'), uint32};
field_data('PLAYER_CHARACTER_POINTS1') -> {16#392 + fields('UNIT_END'), uint32};
field_data('PLAYER_CHARACTER_POINTS2') -> {16#393 + fields('UNIT_END'), uint32};
field_data('PLAYER_TRACK_CREATURES') -> {16#394 + fields('UNIT_END'), uint32};
field_data('PLAYER_TRACK_RESOURCES') -> {16#395 + fields('UNIT_END'), uint32};
field_data('PLAYER_BLOCK_PERCENTAGE') -> {16#396 + fields('UNIT_END'), float};
field_data('PLAYER_DODGE_PERCENTAGE') -> {16#397 + fields('UNIT_END'), float};
field_data('PLAYER_PARRY_PERCENTAGE') -> {16#398 + fields('UNIT_END'), float};
field_data('PLAYER_CRIT_PERCENTAGE') -> {16#399 + fields('UNIT_END'), float};
field_data('PLAYER_RANGED_CRIT_PERCENTAGE') -> {16#39a + fields('UNIT_END'), float};
field_data('PLAYER_EXPLORED_ZONES_1') -> {16#39b + fields('UNIT_END'), uint32};
field_data('PLAYER_REST_STATE_EXPERIENCE') -> {16#3db + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_COINAGE') -> {16#3dc + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_POSSTAT0') -> {16#3DD + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_POSSTAT1') -> {16#3DE + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_POSSTAT2') -> {16#3DF + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_POSSTAT3') -> {16#3E0 + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_POSSTAT4') -> {16#3E1 + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_NEGSTAT0') -> {16#3E2 + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_NEGSTAT1') -> {16#3E3 + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_NEGSTAT2') -> {16#3E4 + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_NEGSTAT3') -> {16#3E5 + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_NEGSTAT4') -> {16#3E6 + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_RESISTANCEBUFFMODSPOSITIVE') -> {16#3E7 + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_RESISTANCEBUFFMODSNEGATIVE') -> {16#3EE + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_MOD_DAMAGE_DONE_POS') -> {16#3F5 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_MOD_DAMAGE_DONE_NEG') -> {16#3FC + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_MOD_DAMAGE_DONE_PCT') -> {16#403 + fields('UNIT_END'), float};
field_data('PLAYER_FIELD_BYTES') -> {16#40A + fields('UNIT_END'), uint8};
field_data('PLAYER_AMMO_ID') -> {16#40B + fields('UNIT_END'), uint32};
field_data('PLAYER_SELF_RES_SPELL') -> {16#40C + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_PVP_MEDALS') -> {16#40D + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_BUYBACK_PRICE_1') -> {16#40E + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_BUYBACK_PRICE_LAST') -> {16#419 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_BUYBACK_TIMESTAMP_1') -> {16#41A + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_BUYBACK_TIMESTAMP_LAST') -> {16#425 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_SESSION_KILLS') -> {16#426 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_YESTERDAY_KILLS') -> {16#427 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_LAST_WEEK_KILLS') -> {16#428 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_THIS_WEEK_KILLS') -> {16#429 + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_THIS_WEEK_CONTRIBUTION') -> {16#42a + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_LIFETIME_HONORABLE_KILLS') -> {16#42b + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_LIFETIME_DISHONORABLE_KILLS') -> {16#42c + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_YESTERDAY_CONTRIBUTION') -> {16#42d + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_LAST_WEEK_CONTRIBUTION') -> {16#42e + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_LAST_WEEK_RANK') -> {16#42f + fields('UNIT_END'), uint32};
field_data('PLAYER_FIELD_BYTES2') -> {16#430 + fields('UNIT_END'), uint8};
field_data('PLAYER_FIELD_WATCHED_FACTION_INDEX') -> {16#431 + fields('UNIT_END'), int32};
field_data('PLAYER_FIELD_COMBAT_RATING_1') -> {16#432 + fields('UNIT_END'), uint32};
field_data('PLAYER_END') -> {16#446 + fields('UNIT_END'), uint32};

field_data('OBJECT_FIELD_CREATED_BY') -> {fields('OBJECT_END') + 16#00, uint64};
field_data('GAMEOBJECT_DISPLAYID') -> {fields('OBJECT_END') + 16#02, uint32};
field_data('GAMEOBJECT_FLAGS') -> {fields('OBJECT_END') + 16#03, uint32};
field_data('GAMEOBJECT_ROTATION') -> {fields('OBJECT_END') + 16#04, float};
field_data('GAMEOBJECT_STATE') -> {fields('OBJECT_END') + 16#08, uint32};
field_data('GAMEOBJECT_POS_X') -> {fields('OBJECT_END') + 16#09, float};
field_data('GAMEOBJECT_POS_Y') -> {fields('OBJECT_END') + 16#0A, float};
field_data('GAMEOBJECT_POS_Z') -> {fields('OBJECT_END') + 16#0B, float};
field_data('GAMEOBJECT_FACING') -> {fields('OBJECT_END') + 16#0C, float};
field_data('GAMEOBJECT_DYN_FLAGS') -> {fields('OBJECT_END') + 16#0D, uint32};
field_data('GAMEOBJECT_FACTION') -> {fields('OBJECT_END') + 16#0E, uint32};
field_data('GAMEOBJECT_TYPE_ID') -> {fields('OBJECT_END') + 16#0F, uint32};
field_data('GAMEOBJECT_LEVEL') -> {fields('OBJECT_END') + 16#10, uint32};
field_data('GAMEOBJECT_ARTKIT') -> {fields('OBJECT_END') + 16#11, uint32};
field_data('GAMEOBJECT_ANIMPROGRESS') -> {fields('OBJECT_END') + 16#12, uint32};
field_data('GAMEOBJECT_PADDING') -> {fields('OBJECT_END') + 16#13, uint32};
field_data('GAMEOBJECT_END') -> {fields('OBJECT_END') + 16#14, uint32};

field_data('DYNAMICOBJECT_CASTER') -> {fields('OBJECT_END') + 16#00, uint64};
field_data('DYNAMICOBJECT_BYTES') -> {fields('OBJECT_END') + 16#02, uint8};
field_data('DYNAMICOBJECT_SPELLID') -> {fields('OBJECT_END') + 16#03, uint32};
field_data('DYNAMICOBJECT_RADIUS') -> {fields('OBJECT_END') + 16#04, float};
field_data('DYNAMICOBJECT_POS_X') -> {fields('OBJECT_END') + 16#05, float};
field_data('DYNAMICOBJECT_POS_Y') -> {fields('OBJECT_END') + 16#06, float};
field_data('DYNAMICOBJECT_POS_Z') -> {fields('OBJECT_END') + 16#07, float};
field_data('DYNAMICOBJECT_FACING') -> {fields('OBJECT_END') + 16#08, float};
field_data('DYNAMICOBJECT_PAD') -> {fields('OBJECT_END') + 16#09, uint32};
field_data('DYNAMICOBJECT_END') -> {fields('OBJECT_END') + 16#0A, uint32};

field_data('CORPSE_FIELDS_OWNER') -> {fields('OBJECT_END') + 16#00, uint64};
field_data('CORPSE_FIELDS_FACING') -> {fields('OBJECT_END') + 16#02, float};
field_data('CORPSE_FIELDS_POS_X') -> {fields('OBJECT_END') + 16#03, float};
field_data('CORPSE_FIELDS_POS_Y') -> {fields('OBJECT_END') + 16#04, float};
field_data('CORPSE_FIELDS_POS_Z') -> {fields('OBJECT_END') + 16#05, float};
field_data('CORPSE_FIELDS_DISPLAY_ID') -> {fields('OBJECT_END') + 16#06, uint32};
field_data('CORPSE_FIELDS_ITEM') -> {fields('OBJECT_END') + 16#07, uint32};
field_data('CORPSE_FIELDS_BYTES_1') -> {fields('OBJECT_END') + 16#1A, uint8};
field_data('CORPSE_FIELDS_BYTES_2') -> {fields('OBJECT_END') + 16#1B, uint8};
field_data('CORPSE_FIELDS_GUILD') -> {fields('OBJECT_END') + 16#1C, uint32};
field_data('CORPSE_FIELDS_FLAGS') -> {fields('OBJECT_END') + 16#1D, uint32};
field_data('CORPSE_FIELDS_DYNAMIC_FLAGS') -> {fields('OBJECT_END') + 16#1E, uint32};
field_data('CORPSE_FIELDS_PAD') -> {fields('OBJECT_END') + 16#1F, uint32};
field_data('CORPSE_END') -> {fields('OBJECT_END') + 16#20, uint32}.
