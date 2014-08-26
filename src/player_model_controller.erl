-module(player_model_controller).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([logged_out/2, logged_out/3, logged_in/2, logged_in/3]).
-export([login/2, logout/1]).
-export([create/2, enum/1, delete/2]).

-include("include/binary.hrl").
-include("include/types.hrl").
-include("include/unit.hrl").
-include("include/shared_defines.hrl").
-include("include/object.hrl").
-include("include/character.hrl").
-include("include/database_records.hrl").


-record(state, {
	account_id,
	guid
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

logout(AccountId) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_fsm:send_event(Pid, logout).


%% behavior callbacks

start_link(AccountId) ->
    gen_fsm:start_link(?MODULE, {AccountId}, []).

init({AccountId}) ->
	io:format("starting player_model_controller~n"),

	util:reg_proc(?MODULE, AccountId),
	{ok, logged_out, #state{account_id=AccountId}}.


% async
logged_out({login, Guid}, State = #state{}) ->
	char_sess:create(Guid),

	{next_state, logged_in, State#state{guid=Guid}};
logged_out(_, State) ->
	{next_state, logged_out, State}.

% sync
logged_out({create, Data}, _From, State = #state{account_id=AccountId}) ->
	Guid = world:get_guid(?highguid_player, 0),
	{CharName, CharMisc, CharMv, Values, Spells, ActionButtons} = create_char_values(Data, Guid),
	%io:format("storing char name: ~p under player name: ~p~n", [Name, PlayerName]),
	char_data:create_char(Guid, AccountId, CharName, CharMisc, CharMv, Values, Spells, ActionButtons),
	char_data:equip_starting_items(Guid),

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
logged_in(logout, State = #state{guid=Guid}) ->
	char_sess:delete(Guid),

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
		{'OBJECT_FIELD_GUID', Guid, uint64},
		{'OBJECT_FIELD_TYPE', ObjectType, uint32},
    {'OBJECT_FIELD_SCALE_X', Scale, float},
		{'UNIT_FIELD_BYTES_0', Race, byte_0},
		{'UNIT_FIELD_BYTES_0', Class, byte_1},
		{'UNIT_FIELD_BYTES_0', Gender, byte_2},
		{'UNIT_FIELD_BYTES_0', PowerType, byte_3},
    {'UNIT_FIELD_BYTES_2', Unk3 bor Unk5, byte_1},
    {'UNIT_FIELD_LEVEL', 1, uint32}, % level
    {'UNIT_FIELD_DISPLAYID', ModelId, uint32},
    {'UNIT_FIELD_NATIVEDISPLAYID', NativeModelId, uint32},
    {'UNIT_FIELD_FACTIONTEMPLATE', FactionTemplate, uint32},
    {'UNIT_FIELD_CHARM', 0, uint64},
    {'UNIT_FIELD_CHANNEL_OBJECT', 0, uint64},
    {'UNIT_CHANNEL_SPELL', 0, uint32},
    {'UNIT_FIELD_SUMMON', 0, uint64}, %pet
    {'UNIT_FIELD_TARGET', 0, uint64},
    {'UNIT_FIELD_CHARMEDBY', 0, uint64},
    {'UNIT_FIELD_SUMMONEDBY', 0, uint64},
    {'UNIT_FIELD_CREATEDBY', 0, uint64},
    {'UNIT_FIELD_AURASTATE', 0, uint32},
    {'UNIT_FIELD_MINDAMAGE', 1.0, float},
    {'UNIT_FIELD_MAXDAMAGE', 1.0, float},
    {'UNIT_FIELD_ATTACK_POWER', 0, int32},
    {'UNIT_FIELD_STAT0', Strength, uint32},
    {'UNIT_FIELD_STAT1', Agility, uint32},
    {'UNIT_FIELD_STAT2', Stamina, uint32},
    {'UNIT_FIELD_STAT3', Intellect, uint32},
    {'UNIT_FIELD_STAT4', Spirit, uint32},
    {'UNIT_FIELD_BASE_HEALTH', Health, uint32},
    {'UNIT_FIELD_BASE_MANA', Power, uint32},
    {'UNIT_FIELD_RESISTANCES', 0, {uint32, 0}}, % base armor
    {'UNIT_FIELD_RESISTANCES', 0, {uint32, 1}}, % unknown
    {'UNIT_FIELD_RESISTANCES', 0, {uint32, 2}}, % base fire res
    {'UNIT_FIELD_RESISTANCES', 0, {uint32, 3}}, % base nature res
    {'UNIT_FIELD_RESISTANCES', 0, {uint32, 4}}, % base frost res
    {'UNIT_FIELD_RESISTANCES', 0, {uint32, 5}}, % base shadow res
    {'UNIT_FIELD_RESISTANCES', 0, {uint32, 6}}, % base arcane res
    {'UNIT_FIELD_BASEATTACKTIME', DefaultAttackTime, float},
    {'UNIT_FIELD_OFFHANDATTACKTIME', DefaultAttackTime, float},
    {'UNIT_FIELD_RANGEDATTACKTIME', DefaultAttackTime, float},
    {'UNIT_MOD_CAST_SPEED', 1.0, float},
    {'UNIT_FIELD_MAXPOWER1', Mana, uint32}, %mana
    {'UNIT_FIELD_MAXPOWER2', Rage, uint32}, %rage
    {'UNIT_FIELD_MAXPOWER3', 0, uint32}, %focus
    {'UNIT_FIELD_MAXPOWER4', Energy, uint32}, %energy
    {'UNIT_FIELD_MAXPOWER5', 0, uint32}, %happiness
    {'UNIT_FIELD_POWER1', Mana, uint32},
    {'UNIT_FIELD_POWER2', Rage, uint32},
    {'UNIT_FIELD_POWER3', 0, uint32},
    {'UNIT_FIELD_POWER4', Energy, uint32},
    {'UNIT_FIELD_POWER5', 0, uint32},
    {'UNIT_FIELD_MAXHEALTH', Health, uint32},
    {'UNIT_FIELD_FLAGS', 16#0008, uint32}, % flags like non-selectable, non-movable, taxi-flight, silecenced, non-attackable, many more
    {'UNIT_FIELD_HEALTH', Health, uint32},
    {'UNIT_FIELD_BYTES_1', 16#EE, byte_1},
    {'PLAYER_EXPLORED_ZONES_1', 0, uint64},
    {'PLAYER_FIELD_COINAGE', 0, uint32},
    {'PLAYER_BYTES', Skin, byte_0},
    {'PLAYER_BYTES', Face, byte_1},
    {'PLAYER_BYTES', HairStyle, byte_2},
    {'PLAYER_BYTES', HairColor, byte_3},
    {'PLAYER_BYTES_2', FacialHair, byte_0},
    {'PLAYER_BYTES_2', 2, byte_3}, %rest state
    {'PLAYER_BYTES_3', Gender, uint16_0}, % (drunk band 16#FFFE) bor Gender
    {'PLAYER_BYTES_3', 0, byte_3}, % battlefield arena faction
    {'PLAYER_FLAGS', PlayerFlags, uint32},
    {'PLAYER_FIELD_WATCHED_FACTION_INDEX', -1, int32},
    {'PLAYER_FIELD_BYTES', 0, byte_2},
    {'PLAYER_CHARACTER_POINTS2', 2, uint32}, %num primary trade professions
    {'PLAYER_FARSIGHT', 0, uint64},
    {'PLAYER_TRACK_CREATURES', 0, uint32},
    {'PLAYER_TRACK_RESOURCES', 0, uint32},
    {'PLAYER_DUEL_ARBITER', 0, uint64},
    {'PLAYER_DUEL_TEAM', 0, uint32},
    {'PLAYER_NEXT_LEVEL_XP', 10, uint32}, % xp to next level
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, float},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 1}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 2}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 3}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 4}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 5}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 6}},

    {'PLAYER_FIELD_POSSTAT0', 0.0, float}, % stat buff mods for strength
    {'PLAYER_FIELD_POSSTAT1', 0.0, float}, % stat buff mods for agi
    {'PLAYER_FIELD_POSSTAT2', 0.0, float}, % stat buff mods sta
    {'PLAYER_FIELD_POSSTAT3', 0.0, float}, % stat buff mods int
    {'PLAYER_FIELD_POSSTAT4', 0.0, float}, % stat buff mods spi
    {'PLAYER_FIELD_NEGSTAT0', 0.0, float}, % stat buff mods for strength
    {'PLAYER_FIELD_NEGSTAT1', 0.0, float}, % stat buff mods
    {'PLAYER_FIELD_NEGSTAT2', 0.0, float}, % stat buff mods
    {'PLAYER_FIELD_NEGSTAT3', 0.0, float}, % stat buff mods
    {'PLAYER_FIELD_NEGSTAT4', 0.0, float}, % stat buff mods

    {'PLAYER_FIELD_RESISTANCEBUFFMODSPOSITIVE', 0.0, {float, 0}}, % armor mod
    {'PLAYER_FIELD_RESISTANCEBUFFMODSPOSITIVE', 0.0, {float, 1}}, % not sure
    {'PLAYER_FIELD_RESISTANCEBUFFMODSPOSITIVE', 0.0, {float, 2}}, % fire mod
    {'PLAYER_FIELD_RESISTANCEBUFFMODSPOSITIVE', 0.0, {float, 3}}, % nature mod
    {'PLAYER_FIELD_RESISTANCEBUFFMODSPOSITIVE', 0.0, {float, 4}}, % frost mod
    {'PLAYER_FIELD_RESISTANCEBUFFMODSPOSITIVE', 0.0, {float, 5}}, % shadow mod
    {'PLAYER_FIELD_RESISTANCEBUFFMODSPOSITIVE', 0.0, {float, 6}}, % arcane mod

    {'PLAYER_FIELD_RESISTANCEBUFFMODSNEGATIVE', 0.0, {float, 0}},
    {'PLAYER_FIELD_RESISTANCEBUFFMODSNEGATIVE', 0.0, {float, 1}},
    {'PLAYER_FIELD_RESISTANCEBUFFMODSNEGATIVE', 0.0, {float, 2}},
    {'PLAYER_FIELD_RESISTANCEBUFFMODSNEGATIVE', 0.0, {float, 3}},
    {'PLAYER_FIELD_RESISTANCEBUFFMODSNEGATIVE', 0.0, {float, 4}},
    {'PLAYER_FIELD_RESISTANCEBUFFMODSNEGATIVE', 0.0, {float, 5}},
    {'PLAYER_FIELD_RESISTANCEBUFFMODSNEGATIVE', 0.0, {float, 6}}

	],

	% create actual data objects

	EmptyValues = char_values:get_empty_values(),
	Values = lists:foldl(fun object_values:set_key_values/2, EmptyValues, KeyValues),
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
	Values = char_data:get_values(Guid),
	Name = char_data:get_char_name(Guid),

	Guid = char_values:get(guid, Values),
	Race = char_values:get(race, Values),
	Class = char_values:get(class, Values),
	Gender = char_values:get(gender, Values),

	Skin = char_values:get(skin, Values),
	Face = char_values:get(face, Values),
	HairStyle = char_values:get(hair_style, Values),
	HairColor = char_values:get(hair_color, Values),
	FacialHair = char_values:get(facial_hair, Values),
	Level = char_values:get(level, Values),

	GuildId = char_values:get(guild_id, Values),

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
	

