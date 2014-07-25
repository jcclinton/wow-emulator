-module(character).
-export([enum/1, create/1, delete/1, logout/1, login/1, update_account_data/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").
-include("include/types.hrl").



update_account_data(_Data) ->
	% dont need to do anything
	ok.

enum(Data) ->
	AccountId = recv_data:get(account_id, Data),
	Chars = char_data:enum_chars(AccountId),
	Num = length(Chars),
	CharDataOut = if Num > 0 ->
								CharList = lists:map(fun mapCharData/1, Chars),
								CharData = iolist_to_binary(CharList),
								CharData;
							Num == 0 -> <<>>
						end,
	Msg = <<Num?B, CharDataOut/binary>>,
	{smsg_char_enum, Msg}.


delete(Data) ->
	Packet = recv_data:get(payload, Data),
	<<Guid?Q>> = Packet,
	char_data:delete_char(Guid),

	Success = 16#39,
	Msg = <<Success?B>>,
	{smsg_char_delete, Msg}.


create(Data) ->
	Guid = world:get_guid(),
	{Char, Values} = create_char_values(Data, Guid),
	AccountId = recv_data:get(account_id, Data),
	%io:format("storing char name: ~p under player name: ~p~n", [Name, PlayerName]),
	char_data:create_char(Guid, AccountId, Char, Values),
	Result = 16#2E, % success
	Msg = <<Result?B>>,
	{smsg_char_create, Msg}.


logout(Data) ->
	AccountId = recv_data:get(account_id, Data),
	Guid = recv_data:get(guid, Data),
	Reason = 0, %0 means is ok to logout
	Wait = 16777216, % set to 0 to set wait time on logout
	Msg = <<Reason?B, Wait?L>>,
	player_controller:send(AccountId, smsg_logout_response, Msg),
	player_controller:send(AccountId, smsg_logout_complete, <<>>),

	ok = world:remove_from_map(Guid),

	player_controller:logout_char(AccountId, Guid),
	ok.


login(Data) ->
	AccountId = recv_data:get(account_id, Data),
	<<Guid?Q>> = recv_data:get(payload, Data),
	player_controller:login_char(AccountId, Guid),



	Char = char_data:get_char(Guid),
	%io:format("logging in ~p~n", [CharName]),
	X = Char#char.x,
	Y = Char#char.y,
	Z = Char#char.z,
	MapId = Char#char.map,
	Orientation = Char#char.orient,
	Payload = <<MapId?L, X?f, Y?f, Z?f, Orientation?f>>,
	%io:format("login payload: ~p~n", [Payload]),
	player_controller:send(AccountId, smsg_login_verify_world, Payload),


	%login packets to send before player is added to map
	Data1 = recv_data:add_value(Data, guid, Guid),
	Data2 = recv_data:add_value(Data1, char, Char),

	Funs = [
		account_data_times(Data2),
		send_motd(Data2),
		set_rest_start(Data2),
		bind_point_update(Data2),
		set_tutorial_flags(Data2),

		initial_spells(Data2),

		%send_unlearn_spells(Data2),
		action_buttons(Data2), % differs
		initialize_factions(Data2), % differs
		init_world_state(Data2),
		login_settimespeed(Data2)
	],

	lists:foreach(fun({OpAtomIn, PayloadIn}) ->
		player_controller:send(AccountId, OpAtomIn, PayloadIn)
	end, Funs),



	ok = world:add_to_map({AccountId, Guid}),

	ok.



send_motd(_Data) ->
	Type = 16#0a,
	Lang = 0,
	Guid = 0,
	ChatMsg = <<"Hello dude">>,
	Len = byte_size(ChatMsg) + 1,
	ChatTag = 0,
	Payload = <<Type?B, Lang?L, Guid?Q, Len?L, ChatMsg/binary, 0?B, ChatTag?B>>,
	{smsg_messagechat, Payload}.

account_data_times(_Data) ->
	% send 32 empty 32 bit words
	Payload = binary:copy(<<0?L>>, 32),
	{smsg_account_data_times, Payload}.

set_rest_start(_Data) ->
	%GameTime = game_time(),
	GameTime = 0,
	Payload = <<GameTime?L>>,
	{smsg_set_rest_start, Payload}.

set_tutorial_flags(_Data) ->
	Payload = binary:copy(<<16#FFFFFFFF?L>>, 8),
	{smsg_tutorial_flags, Payload}.

bind_point_update(Data) ->
	Guid = recv_data:get(guid, Data),
	Char = char_data:get_char(Guid),
	Zone = Char#char.zone,
	Map = Char#char.map,
	X = Char#char.x,
	Y = Char#char.y,
	Z = Char#char.z,
	Payload = <<X?f, Y?f, Z?f, Map?L, Zone?L>>,
	{smsg_bindpointupdate, Payload}.

initial_spells(_Data) ->
	Unk = 0,
	NumSpells = 0,
	NumSpellsOnCooldown = 0,
	Payload = <<Unk?B, NumSpells?W, NumSpellsOnCooldown?W>>,
	{smsg_initial_spells, Payload}.

%send_unlearn_spells(_Data) ->
	%Payload = <<0?L>>,
	%{smsg_send_unlearn_spells, Payload}.

action_buttons(_Data) ->
	Size = 120,
	Payload = binary:copy(<<0?L>>, Size),
	{smsg_action_buttons, Payload}.

initialize_factions(_Data) ->
	Factions = binary:copy(<<0?Q>>, 40),
	Payload = <<16#40?L, Factions/binary>>,
	{smsg_initialize_factions, Payload}.

login_settimespeed(_Data) ->
	GameTime = util:game_time(),
	Speed = util:game_speed(),
	Payload = <<GameTime?L, Speed?f>>,
	{smsg_login_settimespeed, Payload}.

init_world_state(Data) ->
	Guid = recv_data:get(guid, Data),
	Char = char_data:get_char(Guid),
	MapId = Char#char.map,
	ZoneId = Char#char.zone,
	Count = 6,
	%Payload = <<MapId?L, ZoneId?L, Count?W, 16#8d8?L, 0?L, 16#8d7?L, 0?L, 16#8d6?L, 0?L, 16#8d5?L, 0?L, 16#8d4?L, 0?L, 16#8d3?L, 0?L>>,
	Rest = <<16#d808000000000000d708000000000000d608000000000000d508000000000000d408000000000000d308000000000000:384/unsigned-big-integer>>,
	Payload = <<MapId?L, ZoneId?L, Count?W, Rest/binary>>,
	{smsg_init_world_states, Payload}.

			
%%%%%%%%%%%%
%% private







extract_name(Payload) ->
	extract_name(Payload, []).
extract_name(<<0?B, Rest/binary>>, Name) ->
	NameBin = iolist_to_binary(lists:reverse(Name)),
	{NameBin, Rest};
extract_name(<<Char?B, Rest/binary>>, Name) ->
	extract_name(Rest, [Char|Name]).
	

mapCharData({Char, Values}) ->

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

	Zone = Char#char.zone,
	Map = Char#char.map,
	X = Char#char.x,
	Y = Char#char.y,
	Z = Char#char.z,
	Name = Char#char.name,
	AtLoginFlags = Char#char.at_login_flags,

	GeneralFlags = 16#10A00040,

	PetDisplayId = 0,
	PetLevel = 0,
	PetFamily = 0,

	EQUIPMENT_SLOT_END = 19,
	ItemSlotData = binary:copy(<<0?B>>, 5 * EQUIPMENT_SLOT_END),

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
	

create_char_values(Data, Guid) ->
	Payload = recv_data:get(payload, Data),
	{Name, NewPayload} = extract_name(Payload),
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
	Unk3 = 16#08,
	Unk5 = 16#20,
	ModelId = CreateInfo#char_create_info.display_id + Gender,
	NativeModelId = CreateInfo#char_create_info.display_id + Gender,
	Scale = CreateInfo#char_create_info.scale,

	Strength = erlang:round(CreateInfo#char_create_info.strength),
	Agility = erlang:round(CreateInfo#char_create_info.agility),
	Stamina = erlang:round(CreateInfo#char_create_info.stamina),
	Intellect = erlang:round(CreateInfo#char_create_info.intellect),
	Spirit = erlang:round(CreateInfo#char_create_info.spirit),

	FactionTemplate = CreateInfo#char_create_info.faction_template,

	Health = CreateInfo#char_create_info.health,
	Power = CreateInfo#char_create_info.power,
	{Mana, Rage, Energy} = case CreateInfo#char_create_info.power_type of
		rage -> {0, Power, 0};
		energy -> {0, 0, Power};
		_ -> {Power, 0, 0}
	end,

	% ffa
	PlayerFlags = 16#80,




	%initial values for this chars values object
	KeyValues = [
		{'OBJECT_FIELD_GUID', Guid, uint64},
		{'OBJECT_FIELD_TYPE', ObjectType, uint32},
		{'UNIT_FIELD_BYTES_0', Race, byte_0},
		{'UNIT_FIELD_BYTES_0', Class, byte_1},
		{'UNIT_FIELD_BYTES_0', Gender, byte_2},
    {'UNIT_FIELD_BYTES_2', Unk3 bor Unk5, byte_1},
    {'UNIT_FIELD_LEVEL', 1, uint32}, % level
    {'PLAYER_EXPLORED_ZONES_1', 0, uint64},
    {'OBJECT_FIELD_SCALE_X', Scale, float},
    {'UNIT_FIELD_DISPLAYID', ModelId, uint32},
    {'UNIT_FIELD_NATIVEDISPLAYID', NativeModelId, uint32},
    {'PLAYER_FIELD_COINAGE', 0, uint32},
    {'PLAYER_BYTES', Skin, byte_0},
    {'PLAYER_BYTES', Face, byte_1},
    {'PLAYER_BYTES', HairStyle, byte_2},
    {'PLAYER_BYTES', HairColor, byte_3},
    {'PLAYER_BYTES_2', FacialHair, byte_0},
    {'PLAYER_BYTES_2', 2, byte_3}, %rest state
    {'PLAYER_BYTES_3', 0, uint16_0}, %drunk
    {'PLAYER_FLAGS', PlayerFlags, uint32},
    {'PLAYER_FIELD_WATCHED_FACTION_INDEX', 16#ffff, uint32},
    {'PLAYER_FIELD_BYTES', 0, byte_2},
    {'UNIT_FIELD_FACTIONTEMPLATE', FactionTemplate, uint32}, %not sure what this should be
    {'UNIT_FIELD_CHARM', 0, uint64}, %not sure what this should be
    {'PLAYER_CHARACTER_POINTS2', 2, uint32}, %num primary trade professions
    {'UNIT_FIELD_CHANNEL_OBJECT', 0, uint64},
    {'UNIT_CHANNEL_SPELL', 0, uint32},
    {'UNIT_FIELD_SUMMON', 0, uint64}, %pet
    {'UNIT_FIELD_TARGET', 0, uint64},
    {'UNIT_FIELD_CHARMEDBY', 0, uint64},
    {'UNIT_FIELD_SUMMONEDBY', 0, uint64},
    {'UNIT_FIELD_CREATEDBY', 0, uint64},
    {'PLAYER_FARSIGHT', 0, uint64},
    {'PLAYER_TRACK_CREATURES', 0, uint32},
    {'PLAYER_TRACK_RESOURCES', 0, uint32},
    {'PLAYER_DUEL_ARBITER', 0, uint64},
    {'PLAYER_DUEL_TEAM', 0, uint32},
    {'PLAYER_NEXT_LEVEL_XP', 10, uint32}, % xp to next level
    {'UNIT_FIELD_AURASTATE', 0, uint32},
    {'UNIT_FIELD_STAT0', Strength, uint32},
    {'UNIT_FIELD_STAT1', Agility, uint32},
    {'UNIT_FIELD_STAT2', Stamina, uint32},
    {'UNIT_FIELD_STAT3', Intellect, uint32},
    {'UNIT_FIELD_STAT4', Spirit, uint32},
    {'UNIT_FIELD_BASE_HEALTH', Health, uint32},
    {'UNIT_FIELD_BASE_MANA', Power, uint32},
    {'UNIT_FIELD_RESISTANCES', 0, uint32},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, float},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 1}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 2}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 3}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 4}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 5}},
    {'PLAYER_FIELD_MOD_DAMAGE_DONE_PCT', 1.0, {float, 6}},
    {'UNIT_FIELD_BASEATTACKTIME', 2000.0, float},
    {'UNIT_FIELD_BASEATTACKTIME', 2000.0, {float, 1}},
    {'UNIT_FIELD_RANGEDATTACKTIME', 2000.0, float},
    {'UNIT_FIELD_MAXPOWER1', Mana, uint32},
    {'UNIT_FIELD_MAXPOWER2', Rage, uint32},
    {'UNIT_FIELD_MAXPOWER3', 0, uint32},
    {'UNIT_FIELD_MAXPOWER4', Energy, uint32},
    {'UNIT_FIELD_MAXPOWER5', 0, uint32},
    {'UNIT_FIELD_POWER1', Mana, uint32},
    {'UNIT_FIELD_POWER2', Rage, uint32},
    {'UNIT_FIELD_POWER3', 0, uint32},
    {'UNIT_FIELD_POWER4', Energy, uint32},
    {'UNIT_FIELD_POWER5', 0, uint32},
    {'UNIT_FIELD_MAXHEALTH', Health, uint32},
    {'UNIT_FIELD_FLAGS', 16#0008, uint32},
    {'UNIT_FIELD_HEALTH', Health, uint32},
    {'UNIT_FIELD_BYTES_1', 16#EE, byte_1}

		%% ignore skills for now
		%% ignore spells
		%% ignore bags
	],

	% create actual data objects

	TotalCount = update_fields:get_total_count(player),
	% create initially empty binary values object
	EmptyValues = binary:copy(<<0?L>>, TotalCount),
	Values = lists:foldl(fun object_values:set_key_values/2, EmptyValues, KeyValues),
	Char = #char{
		x=X,
		y=Y,
		z=Z,
		orient=O,
		name=Name,
		zone = CreateInfo#char_create_info.zone_id,
		map = CreateInfo#char_create_info.map_id,
		at_login_flags = 0
	},
	{Char, Values}.
