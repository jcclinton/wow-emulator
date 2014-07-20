-module(character).
-export([enum/1, create/1, delete/1, logout/1, login/1, update_account_data/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").



update_account_data(_PropList) ->
	% dont need to do anything
	%io:format("received req to update account~n"),
	ok.

enum(PropList) ->
	AccountId = proplists:get_value(account_id, PropList),
	Chars = char_data:enum_chars(AccountId),
	%io:format("looking up player name: ~p~n", [AccountId]),
	%io:format("matched: ~p~n", [Chars]),
	Opcode = opcode_patterns:getNumByAtom(smsg_char_enum),
	Num = length(Chars),
	CharDataOut2 = if Num > 0 ->
								CharList = lists:map(fun mapCharData/1, Chars),
								CharData = iolist_to_binary(CharList),
	%io:format("mapped char data: ~p~n", [CharData]),
								CharData;
							true -> <<>>
						end,
	Msg = <<Opcode?W, Num?B, CharDataOut2/binary>>,
	%io:format("msg: ~p~n", [Msg]),
	player_controller:send(Msg),
	ok.


delete(PropList) ->
	Packet = proplists:get_value(payload, PropList),
	<<Guid?Q>> = Packet,
	CharName = char_data:get_logged_in_char_name(Guid),
	char_data:delete_char(CharName),

	Opcode = opcode_patterns:getNumByAtom(smsg_char_delete),
	Success = 16#39,
	Msg = <<Opcode?W, Success?B>>,
	player_controller:send(Msg),
	ok.


create(PropList) ->
	Guid = world:get_guid(),
	PropList2 = [{guid, Guid} | PropList],
	Char = create_char_record(PropList2),
	Values = create_char_values(PropList2, Char),
	%io:format("storing char name: ~p under player name: ~p~n", [Name, PlayerName]),
	CharData = {Char#char.name, Char#char.account_id, Char#char.id, Char, Values},
	char_data:create_char(CharData),
	Opcode = opcode_patterns:getNumByAtom(smsg_char_create),
	Result = 16#2E, % success
	Msg = <<Opcode?W, Result?B>>,
	player_controller:send(Msg),
	Values.

logout(PropList) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_logout_response),
	Reason = 0, %0 means is ok to logout
	Wait = 16777216, % set to 0 to set wait time on logout
	Msg = <<Opcode?W, Reason?B, Wait?L>>,
	player_controller:send(Msg),
	CompleteOpcode = opcode_patterns:getNumByAtom(smsg_logout_complete),
	Msg2 = <<CompleteOpcode?W>>,
	player_controller:send(Msg2),

	AccountId = proplists:get_value(account_id, PropList),
	world:remove_from_map(AccountId),
	ok.


login(PropList) ->
	<<Guid?Q>> = proplists:get_value(payload, PropList),
	{Char, Values} = char_data:get_char_data(Guid),
	%io:format("logging in ~p~n", [CharName]),
	X = Char#char.position_x,
	Y = Char#char.position_y,
	Z = Char#char.position_z,
	MapId = Char#char.map_id,
	Orientation = Char#char.orientation,
	Opcode = opcode_patterns:getNumByAtom(smsg_login_verify_world),
	Payload = <<MapId?L, X?f, Y?f, Z?f, Orientation?f>>,
	%io:format("login payload: ~p~n", [Payload]),
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),


	%login packets to send before player is added to map
	PropList2 = [{char, Char}|PropList],
	%PropList3 = [{values, Values}|PropList2],
	account_data_times(PropList2),
	send_motd(PropList2),
	set_rest_start(PropList2),
	bind_point_update(PropList2),
	set_tutorial_flags(PropList2),


	initial_spells(PropList2),

	%send_unlearn_spells(PropList2),
	action_buttons(PropList2), % differs
	initialize_factions(PropList2), % differs
	init_world_state(PropList2),
	login_settimespeed(PropList2),


	%login packets to send after player is added to map
	Update = update_object(Char, Values, true),
	player_controller:send(Update),
	AccountId = proplists:get_value(account_id, PropList),
	Update2 = update_object(Char, Values, false),
	world:add_to_map(AccountId),
	world:send_to_all_but_player(Update2, AccountId),
	ok.



send_motd(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_messagechat),
	Type = 16#0a,
	Lang = 0,
	Guid = 0,
	ChatMsg = "Hello dude",
	Len = length(ChatMsg) + 1,
	ChatTag = 0,
	MsgBin = list_to_binary(ChatMsg),
	Payload = <<Type?B, Lang?L, Guid?Q, Len?L, MsgBin/binary, 0?B, ChatTag?B>>,
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.

account_data_times(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_account_data_times),
	% send 32 empty 32 bit words
	Size = 32 * 32,
	Payload = <<0:Size/unsigned-little-integer>>,
	Msg = <<Opcode?W, Payload/binary>>,
	io:format("sending account data times~n"),
	player_controller:send(Msg),
	ok.

set_rest_start(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_set_rest_start),
	%GameTime = game_time(),
	GameTime = 0,
	Payload = <<GameTime?L>>,
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.

set_tutorial_flags(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_tutorial_flags),
	Payload = binary:copy(<<16#FFFFFFFF?L>>, 8),
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.

bind_point_update(Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_bindpointupdate),
	Char = proplists:get_value(char, Proplist),
	Zone = Char#char.zone_id,
	Map = Char#char.map_id,
	X = Char#char.position_x,
	Y = Char#char.position_y,
	Z = Char#char.position_z,
	Payload = <<X?f, Y?f, Z?f, Map?L, Zone?L>>,
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.

initial_spells(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_initial_spells),
	Unk = 0,
	NumSpells = 0,
	NumSpellsOnCooldown = 0,
	Payload = <<Unk?B, NumSpells?W, NumSpellsOnCooldown?W>>,
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.

%send_unlearn_spells(_Proplist) ->
	%Opcode = opcode_patterns:getNumByAtom(smsg_send_unlearn_spells),
	%Payload = <<0?L>>,
	%Msg = <<Opcode?W, Payload/binary>>,
	%player_controller:send(Msg),
	%ok.

action_buttons(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_action_buttons),
	Size = 120,
	Payload = binary:copy(<<0?L>>, Size),
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.

initialize_factions(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_initialize_factions),
	Size = 64 * 5 * 8,
	Payload = <<16#40?L, 0:Size/unsigned-little-integer>>,
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.

login_settimespeed(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_login_settimespeed),
	GameTime = util:game_time(),
	Speed = util:game_speed(),
	Payload = <<GameTime?L, Speed?f>>,
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.

init_world_state(Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_init_world_states),
	Char = proplists:get_value(char, Proplist),
	MapId = Char#char.map_id,
	ZoneId = Char#char.zone_id,
	Count = 6,
	%Payload = <<MapId?L, ZoneId?L, Count?W, 16#8d8?L, 0?L, 16#8d7?L, 0?L, 16#8d6?L, 0?L, 16#8d5?L, 0?L, 16#8d4?L, 0?L, 16#8d3?L, 0?L>>,
	Rest = <<16#d808000000000000d708000000000000d608000000000000d508000000000000d408000000000000d308000000000000:384/unsigned-big-integer>>,
	Payload = <<MapId?L, ZoneId?L, Count?W, Rest/binary>>,
	Msg = <<Opcode?W, Payload/binary>>,
	player_controller:send(Msg),
	ok.

			

update_object(Char, Values, IsSelf) ->
	Block = update_data:block(Char, Values, IsSelf),

	BlockCount = 1,
	HasTransport = 0,
	Payload = <<BlockCount?L, HasTransport?B, Block/binary>>,
	PayloadSize = byte_size(Payload),
	if PayloadSize > 100 ->
			CompressedOpcode = opcode_patterns:getNumByAtom(smsg_compressed_update_object),
			CompressedPayload = update_data:compress(Payload),
			<<CompressedOpcode?W, PayloadSize?L, CompressedPayload/binary>>;
		true ->
			Opcode = opcode_patterns:getNumByAtom(smsg_update_object),
			<<Opcode?W, Payload/binary>>
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
	

create_char_values(Proplist, Char) ->
	Values = proplists:get_value(values, Proplist),
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





		


create_char_record(PropList) ->
	Payload = proplists:get_value(payload, PropList),
	PlayerName = proplists:get_value(account_id, PropList),
	Guid = proplists:get_value(guid, PropList),
	{Name, NewPayload} = extract_name(Payload),
    <<Race?B, Class?B, Gender?B, Skin?B,
      Face?B, HS?B, HC?B, FH?B, _?B>> = NewPayload,
    RaceName    = char_helper:to_race(Race),
    ClassName   = char_helper:to_class(Class),
    CreateInfo  = content:char_create_info(RaceName, ClassName),
		Realm = 1,
    GenderValue = Gender * if Race =:= 10 -> -1; true -> 1 end,
    Char = #char{id               = Guid,
                 account_id       = PlayerName,
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

