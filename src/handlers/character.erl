-module(character).
-export([enum/2, create/2, delete/2, logout/2, login/2, update_account_data/2]).

-include("include/binary.hrl").
-include("include/database_records.hrl").



update_account_data(_Data, _AccountId) ->
	% dont need to do anything
	%io:format("received req to update account~n"),
	ok.

enum(_Data, AccountId) ->
	Chars = char_data:enum_chars(AccountId),
	%io:format("looking up player name: ~p~n", [AccountId]),
	%io:format("matched: ~p~n", [Chars]),
	Num = length(Chars),
	CharDataOut2 = if Num > 0 ->
								CharList = lists:map(fun mapCharData/1, Chars),
								CharData = iolist_to_binary(CharList),
	%io:format("mapped char data: ~p~n", [CharData]),
								CharData;
							true -> <<>>
						end,
	Msg = <<Num?B, CharDataOut2/binary>>,
	%io:format("msg: ~p~n", [Msg]),
	player_router:send(AccountId, smsg_char_enum, Msg),
	ok.


delete(Data, AccountId) ->
	Packet = recv_data:get(payload, Data),
	<<Guid?Q>> = Packet,
	CharName = char_data:get_logged_in_char_name(Guid),
	char_data:delete_char(CharName),

	Success = 16#39,
	Msg = <<Success?B>>,
	player_router:send(AccountId, smsg_char_delete, Msg),
	ok.


create(Data, AccountId) ->
	Guid = world:get_guid(),
	Data2 = [{guid, Guid} | Data],
	Char = create_char_record(Data2, AccountId),
	Values = create_char_values(Data2, Char),
	%io:format("storing char name: ~p under player name: ~p~n", [Name, PlayerName]),
	CharData = {Char#char.name, Char#char.account_id, Char#char.id, Char, Values},
	char_data:create_char(CharData),
	Result = 16#2E, % success
	Msg = <<Result?B>>,
	player_router:send(AccountId, smsg_char_create, Msg),
	ok.

logout(Data, AccountId) ->
	Reason = 0, %0 means is ok to logout
	Wait = 16777216, % set to 0 to set wait time on logout
	Msg = <<Reason?B, Wait?L>>,
	player_router:send(AccountId, smsg_logout_response, Msg),
	player_router:send(AccountId, smsg_logout_complete, <<>>),

	world:remove_from_map(AccountId),
	Guid = recv_data:get(guid, Data),
	player_router:logout_char(AccountId, Guid),
	ok.


login(Data, AccountId) ->
	<<Guid?Q>> = recv_data:get(payload, Data),
	player_router:login_char(AccountId, Guid),



	{_CharName, AccountId, Guid, Char, Values} = char_data:get_char_data(Guid),
	%io:format("logging in ~p~n", [CharName]),
	X = Char#char.position_x,
	Y = Char#char.position_y,
	Z = Char#char.position_z,
	MapId = Char#char.map_id,
	Orientation = Char#char.orientation,
	Payload = <<MapId?L, X?f, Y?f, Z?f, Orientation?f>>,
	%io:format("login payload: ~p~n", [Payload]),
	player_router:send(AccountId, smsg_login_verify_world, Payload),


	%login packets to send before player is added to map
	Data2 = [{char, Char}|Data],
	%Data3 = [{values, Values}|Data2],
	account_data_times(Data2, AccountId),
	send_motd(Data2, AccountId),
	set_rest_start(Data2, AccountId),
	bind_point_update(Data2, AccountId),
	set_tutorial_flags(Data2, AccountId),


	initial_spells(Data2, AccountId),

	%send_unlearn_spells(Data2, AccountId),
	action_buttons(Data2, AccountId), % differs
	initialize_factions(Data2, AccountId), % differs
	init_world_state(Data2, AccountId),
	login_settimespeed(Data2, AccountId),


	%login packets to send after player is added to map
	{OpAtom, Update} = update_object(Char, Values, true),
	player_router:send(AccountId, OpAtom, Update),
	{OpAtom2, Update2} = update_object(Char, Values, false),
	world:add_to_map(AccountId),
	world:send_to_all_but_player(OpAtom2, Update2, AccountId),
	ok.



send_motd(_Data, AccountId) ->
	Type = 16#0a,
	Lang = 0,
	Guid = 0,
	ChatMsg = "Hello dude",
	Len = length(ChatMsg) + 1,
	ChatTag = 0,
	MsgBin = list_to_binary(ChatMsg),
	Payload = <<Type?B, Lang?L, Guid?Q, Len?L, MsgBin/binary, 0?B, ChatTag?B>>,
	player_router:send(AccountId, smsg_messagechat, Payload),
	ok.

account_data_times(_Data, AccountId) ->
	% send 32 empty 32 bit words
	Size = 32 * 32,
	Payload = <<0:Size/unsigned-little-integer>>,
	io:format("sending account data times~n"),
	player_router:send(AccountId, smsg_account_data_times, Payload),
	ok.

set_rest_start(_Data, AccountId) ->
	%GameTime = game_time(),
	GameTime = 0,
	Payload = <<GameTime?L>>,
	player_router:send(AccountId, smsg_set_rest_start, Payload),
	ok.

set_tutorial_flags(_Data, AccountId) ->
	Payload = binary:copy(<<16#FFFFFFFF?L>>, 8),
	player_router:send(AccountId, smsg_tutorial_flags, Payload),
	ok.

bind_point_update(Data, AccountId) ->
	Char = recv_data:get(char, Data),
	Zone = Char#char.zone_id,
	Map = Char#char.map_id,
	X = Char#char.position_x,
	Y = Char#char.position_y,
	Z = Char#char.position_z,
	Payload = <<X?f, Y?f, Z?f, Map?L, Zone?L>>,
	player_router:send(AccountId, smsg_bindpointupdate, Payload),
	ok.

initial_spells(_Data, AccountId) ->
	Unk = 0,
	NumSpells = 0,
	NumSpellsOnCooldown = 0,
	Payload = <<Unk?B, NumSpells?W, NumSpellsOnCooldown?W>>,
	player_router:send(AccountId, smsg_initial_spells, Payload),
	ok.

%send_unlearn_spells(_Data, AccountId) ->
	%Payload = <<0?L>>,
	%player_router:send(AccountId, smsg_send_unlearn_spells, Payload),
	%ok.

action_buttons(_Data, AccountId) ->
	Size = 120,
	Payload = binary:copy(<<0?L>>, Size),
	player_router:send(AccountId, smsg_action_buttons, Payload),
	ok.

initialize_factions(_Data, AccountId) ->
	Size = 64 * 5 * 8,
	Payload = <<16#40?L, 0:Size/unsigned-little-integer>>,
	player_router:send(AccountId, smsg_initialize_factions, Payload),
	ok.

login_settimespeed(_Data, AccountId) ->
	GameTime = util:game_time(),
	Speed = util:game_speed(),
	Payload = <<GameTime?L, Speed?f>>,
	player_router:send(AccountId, smsg_login_settimespeed, Payload),
	ok.

init_world_state(Data, AccountId) ->
	Char = recv_data:get(char, Data),
	MapId = Char#char.map_id,
	ZoneId = Char#char.zone_id,
	Count = 6,
	%Payload = <<MapId?L, ZoneId?L, Count?W, 16#8d8?L, 0?L, 16#8d7?L, 0?L, 16#8d6?L, 0?L, 16#8d5?L, 0?L, 16#8d4?L, 0?L, 16#8d3?L, 0?L>>,
	Rest = <<16#d808000000000000d708000000000000d608000000000000d508000000000000d408000000000000d308000000000000:384/unsigned-big-integer>>,
	Payload = <<MapId?L, ZoneId?L, Count?W, Rest/binary>>,
	player_router:send(AccountId, smsg_init_world_states, Payload),
	ok.

			

update_object(Char, Values, IsSelf) ->
	Block = update_data:block(Char, Values, IsSelf),

	BlockCount = 1,
	HasTransport = 0,
	Payload = <<BlockCount?L, HasTransport?B, Block/binary>>,
	PayloadSize = byte_size(Payload),
	if PayloadSize > 100 ->
			CompressedPayload = update_data:compress(Payload),
			{smsg_compressed_update_object, <<PayloadSize?L, CompressedPayload/binary>>};
		true ->
			{smsg_update_object, Payload}
	end.






%%%%%%%%%%%%
%% private

extract_name(Payload) ->
	extract_name(Payload, []).
extract_name(<<0?B, Rest/binary>>, Name) ->
	NameBin = iolist_to_binary(lists:reverse(Name)),
	{NameBin, Rest};
extract_name(<<Char?B, Rest/binary>>, Name) ->
	extract_name(Rest, [Char|Name]).
	

mapCharData({_CharName, _AccountName, _, #char{id=Guid, name=Name, race=RaceName, class=ClassName, gender=GenderName, skin=Skin, face=Face, hair_style=HairStyle, hair_color=HairColor, facial_hair=FacialHair, level=Level, zone_id=Zone, map_id=Map, position_x=X, position_y=Y, position_z=Z, guild_id=GuildId, general_flags=GeneralFlags, at_login_flags=AtLoginFlags}, _Values}) ->
	Race = char_helper:race(RaceName),
	Class = char_helper:class(ClassName),
	Gender = char_helper:gender(GenderName),
	PetDisplayId = 0,
	PetLevel = 0,
	PetFamily = 0,
	EQUIPMENT_SLOT_END = 19,
	SlotDataSize = EQUIPMENT_SLOT_END * 40,
	BagDisplayId = 0,
	BagInventoryType = 0,
	NameSize = size(Name) * 8,
	<<NameNum:NameSize/unsigned-big-integer>> = Name,
	<<Guid?Q,
	NameNum:NameSize/unsigned-big-integer,
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
	0:SlotDataSize/unsigned-little-integer,
	BagDisplayId?L,
	BagInventoryType?B>>.
	

create_char_values(_Data, Char) ->
	TotalCount = update_fields:get_total_count(player),
	Values = binary:copy(<<0?L>>, TotalCount),

	Guid = Char#char.id,
	RaceName = Char#char.race,
	ClassName = Char#char.class,
	GenderName = Char#char.gender,
	Race = char_helper:race(RaceName),
	Class = char_helper:class(ClassName),
	Gender = char_helper:gender(GenderName),

	Skin = Char#char.skin,
	Face = Char#char.face,
	HairStyle = Char#char.hair_style,
	HairColor = Char#char.hair_color,
	FacialHair = Char#char.facial_hair,
	
	ObjectType = 25,
	Unk3 = 16#08,
	Unk5 = 16#20,
	ModelId = Char#char.model_id + Gender,
	NativeModelId = Char#char.model_id + Gender,

	Strength = erlang:round(Char#char.strength),
	Agility = erlang:round(Char#char.agility),
	Stamina = erlang:round(Char#char.stamina),
	Intellect = erlang:round(Char#char.intellect),
	Spirit = erlang:round(Char#char.spirit),

	Health = Char#char.health,
	Power = Char#char.power,
	{Mana, Rage, Energy} = case Char#char.power_type of
		rage -> {0, Power, 0};
		energy -> {0, 0, Power};
		_ -> {Power, 0, 0}
	end,

		%Guid2 = Guid + 1,
		PackedGuidBin = <<Guid?G, 0>>,
		%PackedGuidBin = <<41,179,24,0>>,
	<<PackedGuid?L>> = PackedGuidBin,
	%io:format("values packed guid: ~p~n", [PackedGuid]),
	KeyValues = [
		{'OBJECT_FIELD_GUID', PackedGuid, uint64},
		{'OBJECT_FIELD_TYPE', ObjectType, uint32},
		{'UNIT_FIELD_BYTES_0', Race, byte_0},
		{'UNIT_FIELD_BYTES_0', Class, byte_1},
		{'UNIT_FIELD_BYTES_0', Gender, byte_2},
    {'UNIT_FIELD_BYTES_2', Unk3 bor Unk5, byte_1},
    {'UNIT_FIELD_LEVEL', 1, uint32},
    {'PLAYER_EXPLORED_ZONES_1', 0, uint64},
    {'OBJECT_FIELD_SCALE_X', Char#char.scale, float},
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
    {'PLAYER_FLAGS', 0, uint32},
    {'PLAYER_FIELD_WATCHED_FACTION_INDEX', 16#ffff, uint32},
    {'PLAYER_FIELD_BYTES', 0, byte_2},
    {'UNIT_FIELD_FACTIONTEMPLATE', 35, uint32}, %not sure what this should be
    {'UNIT_FIELD_CHARM', 0, uint64}, %not sure what this should be
    {'PLAYER_CHARACTER_POINTS2', 2, uint32}, %num primary trade professions
    {'UNIT_FIELD_CHANNEL_OBJECT', Guid, uint64},
    {'UNIT_CHANNEL_SPELL', 0, uint32},
    {'UNIT_FIELD_SUMMON', 0, uint64}, %pet
    {'UNIT_FIELD_TARGET', Guid, uint64},
    {'UNIT_FIELD_CHARMEDBY', Guid, uint64},
    {'UNIT_FIELD_SUMMONEDBY', Guid, uint64},
    {'UNIT_FIELD_CREATEDBY', Guid, uint64},
    {'PLAYER_FARSIGHT', 0, uint64},
    {'PLAYER_TRACK_CREATURES', 0, uint32},
    {'PLAYER_TRACK_RESOURCES', 0, uint32},
    {'PLAYER_DUEL_ARBITER', Guid, uint64},
    {'PLAYER_DUEL_TEAM', 0, uint32},
    {'PLAYER_NEXT_LEVEL_XP', 10, uint32}, %dont know what this value is supposed to be
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
	lists:foldl(fun object_values:set_key_values/2, Values, KeyValues).





		


create_char_record(Data, AccountId) ->
	Payload = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	{Name, NewPayload} = extract_name(Payload),
    <<Race?B, Class?B, Gender?B, Skin?B,
      Face?B, HS?B, HC?B, FH?B, _?B>> = NewPayload,
    RaceName    = char_helper:to_race(Race),
    ClassName   = char_helper:to_class(Class),
    CreateInfo  = content:char_create_info(RaceName, ClassName),
		Realm = 1,
    GenderValue = Gender * if Race =:= 10 -> -1; true -> 1 end,
    Char = #char{id               = Guid,
                 account_id       = AccountId,
                 realm_id         = Realm,
                 name             = Name,
                 race             = RaceName, 
                 gender           = char_helper:to_gender(Gender),
                 class            = ClassName,
                 skin             = Skin, 
                 face             = Face, 
                 hair_style       = HS, 
                 hair_color       = HC,
                 facial_hair      = FH, 
                 level            = 1,
                 guild_id         = 0,
                 general_flags    = 16#10A00040,
                 at_login_flags   = 0,
                 model_id = CreateInfo#char_create_info.display_id, 
                 faction_template = CreateInfo#char_create_info.faction_template, 
                 map_id           = CreateInfo#char_create_info.map_id, 
                 zone_id          = CreateInfo#char_create_info.zone_id, 
                 position_x       = CreateInfo#char_create_info.position_x, 
                 position_y       = CreateInfo#char_create_info.position_y, 
                 position_z       = CreateInfo#char_create_info.position_z, 
                 orientation      = CreateInfo#char_create_info.orientation, 
                 display_id       = CreateInfo#char_create_info.display_id + GenderValue, 
                 strength         = CreateInfo#char_create_info.strength, 
                 agility          = CreateInfo#char_create_info.agility,
                 stamina          = CreateInfo#char_create_info.stamina, 
                 intellect        = CreateInfo#char_create_info.intellect, 
                 spirit           = CreateInfo#char_create_info.spirit, 
                 health           = CreateInfo#char_create_info.health, 
                 mana             = CreateInfo#char_create_info.mana, 
                 focus            = CreateInfo#char_create_info.focus, 
                 power            = CreateInfo#char_create_info.power, 
                 power_type       = CreateInfo#char_create_info.power_type, 
                 intro            = CreateInfo#char_create_info.intro,
                 attack_power     = CreateInfo#char_create_info.attack_power, 
                 min_dmg          = CreateInfo#char_create_info.min_dmg, 
                 max_dmg          = CreateInfo#char_create_info.max_dmg, 
                 scale            = CreateInfo#char_create_info.scale},
		Char.


