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

-module(player_model_controller).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([logged_out/2, logged_out/3, logged_in/2, logged_in/3]).
-export([login/2, logout/1]).
-export([create/2, enum/1, delete/2]).
-export([login_complete/1]).
-export([update/0]).

-include("include/binary.hrl").
-include("include/types.hrl").
-include("include/unit.hrl").
-include("include/shared_defines.hrl").
-include("include/object.hrl").
-include("include/character.hrl").
-include("include/database_records.hrl").


-record(state, {
	account_id,
	guid,
	parent_pid
}).

%% public api
create(AccountId, Data) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_fsm:sync_send_event(Pid, {create, Data}).

enum(AccountId) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_fsm:sync_send_event(Pid, enum).

delete(AccountId, Guid) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_fsm:sync_send_event(Pid, {delete, Guid}).



login(AccountId, Guid) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_fsm:send_event(Pid, {login, Guid}).

login_complete(Guid) ->
	Pid = util:get_pid(?MODULE, Guid),
	gen_fsm:send_event(Pid, {login_complete, Guid}).

logout(AccountId) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_fsm:send_event(Pid, logout).


update() -> ok.



%% behavior callbacks

start_link(AccountId, ParentPid) ->
    gen_fsm:start_link(?MODULE, {AccountId, ParentPid}, []).

init({AccountId, ParentPid}) ->
	io:format("starting player_model_controller~n"),

	util:reg_proc(?MODULE, AccountId),
	{ok, logged_out, #state{account_id=AccountId, parent_pid=ParentPid}}.


% async
logged_out({login, Guid}, State = #state{parent_pid=ParentPid}) ->
	util:reg_proc(?MODULE, Guid),
	char_sess:create(Guid),

	Name = unit_model_sup,
	Spec = {Name,
		{Name, start_link, [Guid, player]},
		permanent, 2000, supervisor, [Name]},
	{ok, _Pid} = supervisor:start_child(ParentPid, Spec),

	{next_state, logged_in, State#state{guid=Guid}};
logged_out(_, State) ->
	{next_state, logged_out, State}.

% sync
logged_out({create, Data}, _From, State = #state{account_id=AccountId}) ->
	Guid = world:get_guid(?highguid_player, 0),
	{CharName, CharMisc, CharMv, Values, Spells, ActionButtons} = create_char_values(Data, Guid),
	%io:format("storing char name: ~p under player name: ~p~n", [Name, PlayerName]),
	char_data:create_char(Guid, AccountId, CharName, CharMisc, CharMv, Values, Spells, ActionButtons),
	% TODO uncomment this once all values are switched over
	%char_data:equip_starting_items(Guid),

	Result = 16#2E, % success
	{reply, Result, logged_out, State};
logged_out(enum, _From, State = #state{account_id=AccountId}) ->
	CharGuids = char_data:enum_char_guids(AccountId),
	Num = length(CharGuids),
	CharDataOut = if Num > 0 ->
								CharList = lists:map(fun mapCharGuids/1, CharGuids),
								iolist_to_binary(CharList);
							Num == 0 -> <<>>
						end,
	Msg = {Num, CharDataOut},

	{reply, Msg, logged_out, State};
logged_out({delete, Guid}, _From, State = #state{}) ->
	ItemGuids = item:get_item_guids(Guid),
	item_data:delete_items(ItemGuids),
	char_data:delete_char(Guid),
	Result = 16#39,
	{reply, Result, logged_out, State};
logged_out(_, _From, State) ->
	{next_state, logged_out, State}.



% async
logged_in({login_complete, Guid}, State = #state{account_id=AccountId}) ->
	% TODO find a better place to put this, it cant get called until values have been loaded though
	% set player to standing on login
	player_state:set_value(Guid, ?standing, {unit_field_bytes_1, 0}),

	ok = world:add_to_map({AccountId, Guid}),
	{next_state, logged_in, State};
logged_in(logout, State = #state{guid=Guid, parent_pid=ParentPid}) ->
	char_sess:delete(Guid),

	Name = unit_model_sup,
	supervisor:terminate_child(ParentPid, Name),
	supervisor:delete_child(ParentPid, Name),

	util:unreg_proc(?MODULE, Guid),
	{next_state, logged_out, State#state{guid=0}};
logged_in(_, State) ->
	{next_state, logged_in, State}.

% sync
logged_in(_, _From, State) ->
	{next_state, logged_in, State}.



handle_info(_Info, State, Data) ->
	{next_state, State, Data}.

handle_event(_Event, State, Data) ->
	{next_state, State, Data}.

handle_sync_event(_Event, _From, State, Data) ->
	{next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
	ok.

code_change(_OldVsn, State, Data, _Extra) ->
	{ok, State, Data}.


%% private


create_char_values(Payload, Guid) ->
	{Name, NewPayload} = util:extract_string(Payload),
    <<Race?B, Class?B, Gender?B, Skin?B,
      Face?B, HairStyle?B, HairColor?B, FacialHair?B, _?B>> = NewPayload,
    RaceName    = char_helper:to_race(Race),
    ClassName   = char_helper:to_class(Class),
    CreateInfo  = content:char_create_info(RaceName, ClassName),
		X = CreateInfo#char_create_info.position_x,
		Y = CreateInfo#char_create_info.position_y,
		Z = CreateInfo#char_create_info.position_z,
		O = CreateInfo#char_create_info.orientation,


	ObjectType = ?typemask_object bor ?typemask_unit bor ?typemask_player,
	Unk3 = ?unit_byte2_flag_unk3,
	Unk5 = ?unit_byte2_flag_unk5,
	ModelId = CreateInfo#char_create_info.display_id + Gender,
	NativeModelId = CreateInfo#char_create_info.display_id + Gender,
	Scale = if RaceName == tauren ->
			if Gender == ?gender_male -> ?default_tauren_male_scale;
				Gender == ?gender_female -> ?default_tauren_female_scale
			end;
		RaceName /= tauren -> ?default_object_scale
	end,

	Strength = erlang:round(CreateInfo#char_create_info.strength),
	Agility = erlang:round(CreateInfo#char_create_info.agility),
	Stamina = erlang:round(CreateInfo#char_create_info.stamina),
	Intellect = erlang:round(CreateInfo#char_create_info.intellect),
	Spirit = erlang:round(CreateInfo#char_create_info.spirit),

	FactionTemplate = CreateInfo#char_create_info.faction_template,

	Health = CreateInfo#char_create_info.health,
	Power = CreateInfo#char_create_info.power,
	{PowerType, Mana, Rage, Energy} = case CreateInfo#char_create_info.power_type of
		rage -> {?power_rage, 0, Power, 0};
		energy -> {?power_energy, 0, 0, Power};
		_ -> {?power_mana, Power, 0, 0}
	end,

	% ffa
	PlayerFlags = ?player_flags_ffa_pvp,

	DefaultAttackTime = ?default_attack_time,




	%initial values for this chars values object
	KeyValues = [
		{object_field_guid, Guid, uint64},
		{object_field_type, ObjectType, uint32},
    {object_field_scale_x, Scale, float},
		{{unit_field_bytes_0, 0}, Race, uint8},
		{{unit_field_bytes_0, 1}, Class, uint8},
		{{unit_field_bytes_0, 2}, Gender, uint8},
		{{unit_field_bytes_0, 3}, PowerType, uint8},
    {{unit_field_bytes_2, 1}, Unk3 bor Unk5, uint8},
    {unit_field_level, 1, uint32}, % level
    {unit_field_displayid, ModelId, uint32},
    {unit_field_nativedisplayid, NativeModelId, uint32},
    {unit_field_factiontemplate, FactionTemplate, uint32},
    {unit_field_charm, 0, uint64},
    {unit_field_channel_object, 0, uint64},
    {unit_channel_spell, 0, uint32},
    {unit_field_summon, 0, uint64}, %pet
    {unit_field_target, 0, uint64},
    {unit_field_charmedby, 0, uint64},
    {unit_field_summonedby, 0, uint64},
    {unit_field_createdby, 0, uint64},
    {unit_field_aurastate, 0, uint32},
    {unit_field_mindamage, 1.0, float},
    {unit_field_maxdamage, 1.0, float},
    {unit_field_attack_power, 0, int32},
    {unit_field_stat0, Strength, uint32},
    {unit_field_stat1, Agility, uint32},
    {unit_field_stat2, Stamina, uint32},
    {unit_field_stat3, Intellect, uint32},
    {unit_field_stat4, Spirit, uint32},
    {unit_field_base_health, Health, uint32},
    {unit_field_base_mana, Power, uint32},
    {unit_field_resistances, 0, uint32}, % base armor
    {unit_field_resistances_01, 0, uint32}, % unknown
    {unit_field_resistances_02, 0, uint32}, % base fire res
    {unit_field_resistances_03, 0, uint32}, % base nature res
    {unit_field_resistances_04, 0, uint32}, % base frost res
    {unit_field_resistances_05, 0, uint32}, % base shadow res
    {unit_field_resistances_06, 0, uint32}, % base arcane res
    {unit_field_baseattacktime, DefaultAttackTime, float},
    {unit_field_offhandattacktime, DefaultAttackTime, float},
    {unit_field_rangedattacktime, DefaultAttackTime, float},
    {unit_mod_cast_speed, 1.0, float},
    {unit_field_maxpower1, Mana, uint32}, %mana
    {unit_field_maxpower2, Rage, uint32}, %rage
    {unit_field_maxpower3, 0, uint32}, %focus
    {unit_field_maxpower4, Energy, uint32}, %energy
    {unit_field_maxpower5, 0, uint32}, %happiness
    {unit_field_power1, Mana, uint32},
    {unit_field_power2, Rage, uint32},
    {unit_field_power3, 0, uint32},
    {unit_field_power4, Energy, uint32},
    {unit_field_power5, 0, uint32},
    {unit_field_maxhealth, Health, uint32},
    {unit_field_flags, 16#0008, uint32}, % flags like non-selectable, non-movable, taxi-flight, silecenced, non-attackable, many more
    {unit_field_health, Health, uint32},
    {{unit_field_bytes_1, 1}, 16#EE, uint8},
    {player_explored_zones_1, 0, uint64},
    {player_field_coinage, 0, uint32},
    {{player_bytes, 0}, Skin, uint8},
    {{player_bytes, 1}, Face, uint8},
    {{player_bytes, 2}, HairStyle, uint8},
    {{player_bytes, 3}, HairColor, uint8},
    {{player_bytes_2, 0}, FacialHair, uint8},
    {{player_bytes_2, 3}, 2, uint8}, %rest state
    {{player_bytes_3, 0}, Gender, uint16}, % (drunk band 16#FFFE) bor Gender
    {{player_bytes_3, 3}, 0, uint8}, % battlefield arena faction
    {player_flags, PlayerFlags, uint32},
    {player_field_watched_faction_index, -1, int32},
    {{player_field_bytes, 2}, 0, uint8},
    {player_character_points2, 2, uint32}, %num primary trade professions
    {player_farsight, 0, uint64},
    {player_track_creatures, 0, uint32},
    {player_track_resources, 0, uint32},
    {player_duel_arbiter, 0, uint64},
    {player_duel_team, 0, uint32},
    {player_next_level_xp, 10, uint32}, % xp to next level
    {{player_field_mod_damage_done_pct, 0}, 1.0, float},
    {{player_field_mod_damage_done_pct, 1}, 1.0, float},
    {{player_field_mod_damage_done_pct, 2}, 1.0, float},
    {{player_field_mod_damage_done_pct, 3}, 1.0, float},
    {{player_field_mod_damage_done_pct, 4}, 1.0, float},
    {{player_field_mod_damage_done_pct, 5}, 1.0, float},
    {{player_field_mod_damage_done_pct, 6}, 1.0, float},

    {player_field_posstat0, 0.0, float}, % stat buff mods for strength
    {player_field_posstat1, 0.0, float}, % stat buff mods for agi
    {player_field_posstat2, 0.0, float}, % stat buff mods sta
    {player_field_posstat3, 0.0, float}, % stat buff mods int
    {player_field_posstat4, 0.0, float}, % stat buff mods spi
    {player_field_negstat0, 0.0, float}, % stat buff mods for strength
    {player_field_negstat1, 0.0, float}, % stat buff mods
    {player_field_negstat2, 0.0, float}, % stat buff mods
    {player_field_negstat3, 0.0, float}, % stat buff mods
    {player_field_negstat4, 0.0, float}, % stat buff mods

    {{player_field_resistancebuffmodspositive, 0}, 0.0, float}, % armor mod
    {{player_field_resistancebuffmodspositive, 1}, 0.0, float}, % not sure
    {{player_field_resistancebuffmodspositive, 2}, 0.0, float}, % fire mod
    {{player_field_resistancebuffmodspositive, 3}, 0.0, float}, % nature mod
    {{player_field_resistancebuffmodspositive, 4}, 0.0, float}, % frost mod
    {{player_field_resistancebuffmodspositive, 5}, 0.0, float}, % shadow mod
    {{player_field_resistancebuffmodspositive, 6}, 0.0, float}, % arcane mod

    {{player_field_resistancebuffmodsnegative, 0}, 0.0, float},
    {{player_field_resistancebuffmodsnegative, 1}, 0.0, float},
    {{player_field_resistancebuffmodsnegative, 2}, 0.0, float},
    {{player_field_resistancebuffmodsnegative, 3}, 0.0, float},
    {{player_field_resistancebuffmodsnegative, 4}, 0.0, float},
    {{player_field_resistancebuffmodsnegative, 5}, 0.0, float},
    {{player_field_resistancebuffmodsnegative, 6}, 0.0, float}

	],

	% create actual data objects

	EmptyValues = char_values:get_empty_values(),
	Values = lists:foldl(fun object_values:set_value/2, EmptyValues, KeyValues),
	CharMisc = #char_misc{
		at_login_flags = 0
	},
	CharMv = #char_move{
		x=X,
		y=Y,
		z=Z,
		orient=O,
		zone = CreateInfo#char_create_info.zone_id,
		map = CreateInfo#char_create_info.map_id
	},

	Spells = #char_spells{ids=CreateInfo#char_create_info.initial_spells},

	ActionButtons = CreateInfo#char_create_info.initial_action_bars,
	ActionButtonsBin = char_data:create_action_buttons(ActionButtons),


	{Name, CharMisc, CharMv, Values, Spells, ActionButtonsBin}.







mapCharGuids(Guid) ->
	CharMove = char_data:get_char_move(Guid),
	CharMisc = char_data:get_char_misc(Guid),
	Values = char_data:get_stored_values(Guid),
	Name = char_data:get_char_name(Guid),

	Guid = char_values:get_value(object_field_guid, Values),
	Race = char_values:get_value({unit_field_bytes_0, 0}, Values),
	Class = char_values:get_value({unit_field_bytes_0, 1}, Values),
	Gender = char_values:get_value({unit_field_bytes_0, 2}, Values),

	Skin = char_values:get_value({player_bytes, 0}, Values),
	Face = char_values:get_value({player_bytes, 1}, Values),
	HairStyle = char_values:get_value({player_bytes, 2}, Values),
	HairColor = char_values:get_value({player_bytes, 3}, Values),
	FacialHair = char_values:get_value({player_bytes_2, 0}, Values),
	Level = char_values:get_value(unit_field_level, Values),

	GuildId = char_values:get_value(player_guildid, Values),

	Zone = CharMove#char_move.zone,
	Map = CharMove#char_move.map,
	X = CharMove#char_move.x,
	Y = CharMove#char_move.y,
	Z = CharMove#char_move.z,

	AtLoginFlags = CharMisc#char_misc.at_login_flags,

	GeneralFlags = 16#10A00040,

	PetDisplayId = 0,
	PetLevel = 0,
	PetFamily = 0,

	EmptySlot = <<0?L, 0?B>>,
	ItemGuids = item:get_equipped_item_guids(Guid),
	ItemSlotData = lists:foldl(fun(ItemGuid, Acc) ->
		SlotData = try item_data:get_values(ItemGuid) of
			ItemValues ->
				ItemId = item_values:get_item_id(ItemValues),
				ItemProto = content:lookup_item(ItemId),
				if ItemProto /= false ->
						DisplayInfoId = ItemProto#item_proto.display_info_id,
						InvType = ItemProto#item_proto.inventory_type,
						<<DisplayInfoId?L, InvType?B>>;
					ItemProto == false ->
						EmptySlot
				end
			catch
				badarg -> EmptySlot
			end,
			<<Acc/binary, SlotData/binary>>
	end, <<>>, ItemGuids),
	%ItemSlotData = binary:copy(EmptySlot, ?equipment_slot_end),

	BagDisplayId = 0,
	BagInventoryType = 0,

	<<Guid?Q,
	Name/binary,
	0?B,
	Race?B,
	Class?B,
	Gender?B,

	Skin?B,
	Face?B,
	HairStyle?B,
	HairColor?B,
	FacialHair?B,

	Level?B,

	Zone?L,
	Map?L,
	X?f,
	Y?f,
	Z?f,

	GuildId?L,
	GeneralFlags?L,
	AtLoginFlags?B,

	PetDisplayId?L,
	PetLevel?L,
	PetFamily?L,
	ItemSlotData/binary,
	BagDisplayId?L,
	BagInventoryType?B>>.
	

