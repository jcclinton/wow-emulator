-module(character).
-export([enum/1, create/1, login/1]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/database_records.hrl").
-include("include/update.hrl").

enum(PropList) ->
	PlayerName = proplists:get_value(account_id, PropList),
	Chars = ets:match_object(characters, {'_', PlayerName, '_', '_', '_'}),
	%io:format("looking up player name: ~p~n", [PlayerName]),
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
	world_socket_controller:send(Msg),
	ok.


create(PropList) ->
	%Payload = proplists:get_value(payload, PropList),
	%PlayerName = proplists:get_value(account_id, PropList),
	%{Name, NewPayload} = extract_name(Payload),
	%<<Race?B, Class?B, Gender?B, Skin?B, Face?B, HairStyle?B, HairColor?B, FacialHair?B, OutfitId?B>> = NewPayload,
	%Guid = world:get_guid(),
	%Level = 1,
	%Zone = 12,
	%Map = 0,
	%X = -8949.95,
	%Y = -132.493,
	%Z = 83.5312,
	%Orientation = 0,
	%Char = #char{id=Guid, name=Name, race=Race, class=Class, gender=Gender, skin=Skin, face=Face, hair_style=HairStyle, hair_color=HairColor, facial_hair=FacialHair, outfit_id=OutfitId, level=Level, zone_id=Zone, map_id=Map, position_x=X, position_y=Y, position_z=Z, orientation=Orientation},
	Guid = world:get_guid(),
	PropList2 = [{guid, Guid} | PropList],
	Char = create_char_record(PropList2),
	Values = create_char_values(PropList2),
	%io:format("storing char name: ~p under player name: ~p~n", [Name, PlayerName]),
	ets:insert(characters, {Char#char.name, Char#char.account_id, Char#char.id, Char, Values}),
	Opcode = opcode_patterns:getNumByAtom(smsg_char_create),
	Result = 16#2E,
	Msg = <<Opcode?W, Result?B>>,
	world_socket_controller:send(Msg),
	ok.

create_char_values(PropList) ->
	Payload = proplists:get_value(payload, PropList),
	Values = proplists:get_value(values, PropList),
	Guid = proplists:get_value(guid, PropList),
	{_Name, NamelessPayload} = extract_name(Payload),
	<<Race?B, Class?B, Gender?B, Skin?B,
		Face?B, HairStyle?B, HairColor?B, FacialHair?B, _?B>> = NamelessPayload,
	Unk3 = 16#08,
	Unk5 = 16#20,
	ModelId = 1, %not sure what this should be
	NativeModelId = 1, %not sure what this should be
	KeyValues = [
		{'OBJECT_FIELD_GUID', Guid, uint64},
		{'OBJECT_FIELD_TYPE', 3, uint32},
		{'UNIT_FIELD_BYTES_0', Race, byte_0},
		{'UNIT_FIELD_BYTES_0', Class, byte_1},
		{'UNIT_FIELD_BYTES_0', Gender, byte_2},
    {'UNIT_FIELD_BYTES_2', Unk3 bor Unk5, byte_1},
    {'UNIT_FIELD_LEVEL', 0, uint32},
    {'PLAYER_EXPLORED_ZONES_1', 0, uint64},
    {'OBJECT_FIELD_SCALE_X', 1, float},
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
    {'PLAYER_CHARACTER_POINTS2', 10, uint32}, %num primary trade professions
    {'UNIT_FIELD_CHANNEL_OBJECT', Guid, uint64},
    {'UNIT_CHANNEL_SPELL', 0, uint32},
    {'UNIT_FIELD_SUMMON', 0, uint64}, %pet
    {'UNIT_FIELD_TARGET', Guid, uint64},
    {'UNIT_FIELD_CHARMEDBY', Guid, uint64},
    {'UNIT_FIELD_SUMMONEDBY', Guid, uint64},
    {'UNIT_FIELD_CREATEDBY', Guid, uint64},
    {'PLAYER_FARSIGHT', Guid, uint64},
    {'PLAYER_TRACK_CREATURES', 0, uint32},
    {'PLAYER_TRACK_RESOURCES', 0, uint32},
    {'PLAYER_DUEL_ARBITER', Guid, uint64},
    {'PLAYER_DUEL_TEAM', 0, uint32},
    {'PLAYER_NEXT_LEVEL_XP', 10, uint32}, %dont know what this value is supposed to be
    {'UNIT_FIELD_AURASTATE', 0, uint32},
    {'UNIT_FIELD_STAT0', 10, uint32}, %fill in later
    {'UNIT_FIELD_STAT1', 10, uint32}, %fill in later
    {'UNIT_FIELD_STAT2', 10, uint32}, %fill in later
    {'UNIT_FIELD_STAT3', 10, uint32}, %fill in later
    {'UNIT_FIELD_STAT4', 10, uint32}, %fill in later
    {'UNIT_FIELD_BASE_HEALTH', 10, uint32}, %fill in later
    {'UNIT_FIELD_BASE_MANA', 10, uint32}, %fill in later
    {'UNIT_FIELD_RESISTANCES', 10, uint32}, %fill in later
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
    {'UNIT_FIELD_MAXPOWER1', 100, uint32}, %fill in later
    {'UNIT_FIELD_MAXPOWER2', 100, uint32}, %fill in later
    {'UNIT_FIELD_MAXPOWER3', 100, uint32}, %fill in later
    {'UNIT_FIELD_MAXPOWER4', 0, uint32}, %fill in later
    {'UNIT_FIELD_MAXPOWER5', 0, uint32}, %fill in later
    {'UNIT_FIELD_POWER1', 100, uint32}, %fill in later
    {'UNIT_FIELD_POWER2', 100, uint32}, %fill in later
    {'UNIT_FIELD_POWER3', 0, uint32}, %fill in later
    {'UNIT_FIELD_POWER4', 0, uint32}, %fill in later
    {'UNIT_FIELD_POWER5', 100, uint32}, %fill in later
    {'UNIT_FIELD_MAXHEALTH', 120, uint32}, %fill in later
    {'UNIT_FIELD_FLAGS', 16#0008, uint32},
    {'UNIT_FIELD_HEALTH', 120, uint32}, %fill in later
    {'UNIT_FIELD_BYTES_1', 16#EE, byte_1}
		%% ignore skills for now
		%% ignore spells
		%% ignore bags
	],
	NewValues = lists:foldl(fun setKeyValues/2, Values, KeyValues),
	NewValues.

setKeyValues({IndexName, Value, Type}, Values) ->
	case Type of
		uint32 ->
			set_uint32_value(IndexName, Value, Values);
		uint64 ->
			set_uint64_value(IndexName, Value, Values);
		uint16_0 ->
			set_uint16_value(IndexName, Value, Values, 0);
		byte_0 ->
			set_byte_value(IndexName, Value, Values, 0);
		byte_1 ->
			set_byte_value(IndexName, Value, Values, 1);
		byte_2 ->
			set_byte_value(IndexName, Value, Values, 2);
		byte_3 ->
			set_byte_value(IndexName, Value, Values, 3);
		float ->
			set_float_value(IndexName, Value, Values);
		{float, Offset} ->
			set_float_value(IndexName, Value, Values, Offset)
	end.




		


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


                 %general_flags    = 16#10A00040,


login(PropList) ->
	_PlayerName = proplists:get_value(account_id, PropList),
	<<Guid?Q>> = proplists:get_value(payload, PropList),
	[{_,_,Guid,Char, _Values}] = ets:match_object(characters, {'_', '_', Guid, '_', '_'}),
	X = Char#char.position_x,
	Y = Char#char.position_y,
	Z = Char#char.position_z,
	MapId = Char#char.map_id,
	Orientation = Char#char.orientation,
	Opcode = opcode_patterns:getNumByAtom(smsg_login_verify_world),
	Payload = <<MapId?L, X?f, Y?f, Z?f, Orientation?f>>,
	%io:format("login payload: ~p~n", [Payload]),
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),


	%login packets to send before player is added to map
	PropList2 = [{char, Char}|PropList],
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
	update_object(PropList2),
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
	world_socket_controller:send(Msg),
	ok.

account_data_times(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_account_data_times),
	% send 32 empty 32 bit words
	Size = 32 * 32,
	Payload = <<0:Size/unsigned-little-integer>>,
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.

set_rest_start(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_set_rest_start),
	%GameTime = game_time(),
	GameTime = 0,
	Payload = <<GameTime?L>>,
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.

set_tutorial_flags(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_tutorial_flags),
	Payload = <<0?QQ>>,
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
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
	world_socket_controller:send(Msg),
	ok.

initial_spells(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_initial_spells),
	Unk = 0,
	NumSpells = 0,
	NumSpellsOnCooldown = 0,
	Payload = <<Unk?B, NumSpells?W, NumSpellsOnCooldown?W>>,
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.

send_unlearn_spells(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_send_unlearn_spells),
	Payload = <<0?L>>,
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.

action_buttons(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_action_buttons),
	Size = 120 * 32,
	Payload = <<0:Size/unsigned-little-integer>>,
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.

initialize_factions(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_initialize_factions),
	Size = 64 * 5 * 8,
	Payload = <<16#40?L, 0:Size/unsigned-little-integer>>,
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.

login_settimespeed(_Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_login_settimespeed),
	GameTime = game_time(),
	Speed = 0.01666667,
	Payload = <<GameTime?L, Speed?f>>,
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
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
	%Size = 138 * 8,
	%Payload = <<16#82140037 030030f1 0500 0000db6f 9b0fe017 00b80300 30f10000 00006d28 87bf5608 00cb010030f1000000009214aa68510400c5000030f100000000698c44abb6bc00a42e0030f100000000383c907a000000000c0000000600d808000000000000d708000000000000d608000000000000d508000000000000d408000000000000d308000000000000:Size/unsigned-big-integer>>,
	io:format("world init payload: ~p~n", [Payload]),
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.

update_object(Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_update_object),
	Char = proplists:get_value(char, Proplist),
	
        Block = update_helper:block(create_object2, Char),
        Packet = update_helper:packet([Block]),
        Payload = update_helper:message(Packet),

	BlockCount = 1,
	HasTransport = 0,
	%Payload = <<BlockCount?L, HasTransport?B>>,
	%Size = 48 * 8,
	%Payload = <<16#db9e10d10230f129920bc6fe7406c3fca7a6420000000000000000004d030000010000008f8b0bc65caf07c374b3a542:Size/unsigned-big-integer>>,
	%io:format("update payload: ~p~n", [Payload]),
	Msg = <<Opcode?W, BlockCount?L, HasTransport?B, Payload/binary>>,
	%Payload = getPayload(),
	%io:format("payload: ~p~n", [Payload]),
	%Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.

update_object2(Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_update_object),
	Char = proplists:get_value(char, Proplist),
	
        Block = update_helper:block(create_object2, Char),
        UF = Block#update_block.update_flags,
        BB = Block#update_block{ update_flags = lists:delete(self, UF) },
        P = update_helper:packet([BB]),
        Payload = update_helper:message(P),

	%BlockCount = 0,
	%HasTransport = 1,
	%Payload = <<BlockCount?L, HasTransport?B>>,
	Msg = <<Opcode?W, 1?L, 0?B, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.



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

game_time() ->
    {Y, Mo, Dm} = erlang:date(),
    {H, Mi, _} = erlang:time(),
    Dw = calendar:day_of_the_week(Y, Mo, Dm),
    GameTime = (((((Mi band 16#3F) bor 
                   (H*64 band 16#7C0)) bor 
                   (Dw*2048 band 16#3800)) bor 
                   ((Dm - 1)*16384 band 16#FC000)) bor 
                   ((Mo - 1)*1048576 band 16#F00000)) bor 
                   ((Y - 2000)*16777216 band 16#1F000000),
    GameTime.

getPayload() ->
	Payload = <<16#1304000078017553bf4b5b5118fdee7bd144332482adf517bcc1d66a03ad832245cc55411211b1c53d501c057128746adf7ff0824345310a2e2e5d4af71214dc04177110313a3989590a9df49cebbda43ed21bbe1ff7dcefdc7cf9ee494ab0bc68f64b7a49abace2a634454c841882f6e15e1190303ff4eb85d04c615488b3aa22c4506a588324c558dd40685e34176703258660d8af91c4d95e341f67554588a1d4b078779cd50984e6450b4dd8c47068d8efdb90c53a6eb0179fb0390311620886fdd9208d29b5607f8fe5451f9f300353470c89610ef4126a307937cd4f62da89351e0e7f9891e3b3f4d1e8d7c4e1dfccfe34b1c71568911aacacd7fa3610035dcb1435e943cf442ff55791e1f240a47a9938c88c6344ad06b28e9559f3750e555966ef26e40e9fa6abf67bf57cace94973305c493c1e840ce137fa800e4d424feed526b05f8631de4078ccd99e31252a857c136fc46722b6921439b196fab13d899f981f014e2bd52bba54bfd0eab603bb30df05cfb77cfe4f7c6b7381d8fa91a761c5f5015d5c5fd6c976df47d38adaa792a947f6498db8ff8653bbd3ad53a2d39453877beb329aeec11df829f2c6446aa4457236e74815f6c44a36ff84985339b567f715dc41fe8e8dbb36feb4d1435de16a4b975737f49fd38abe1efcae01052fe13887ff5bc6a8f5016dc788588171bce0ee00000078016366606060643a9ebf91e9803c6bd084db8ec78aae053b1b9f7673626068b06704ca6a073a3330ca3b2b3002d500b907e415812484dd607f9d9b814103640410f303710a10331d4fdf0036cd2f27ffe83943962340312040330da8062808310dc26eb03f8ecdb4c31e8120a79973261c3baed4ee02360bdd308f409030c430081bbb6100c0f82abf0761b1bf501b00007801bd590b501457167dd320e2f4a8a82896e83a7e82514141fc247ee8e6a111abfc4465cd661763ac208ee45392e89aa89ba02589b5bb9598504116911599c44f94d26c168d1f20d3e5abb5d0d594568c19d7007ed094a521ab44c5e09edb3d03cdc8e084a2f652b7fbce9bf7eeedf3eeeff5f0040349eecb9d465a62eb8296330b6323e726b135076c22fddd4e5a41ce4eded33eb182663166577b3396c8d82635b3ff66953e57759fa162090b19c8d4b0fe3487c8ce2abe6752f2537d19bbdc898db4b0d8ba2e1886c0e2e3e39511908969a11dcc2c1293703bdd99b10b9d8cfb9d27272b274ff457ca318918a41a9ca5906003eb3ac20c3d25d3ab95e4ea7eaae4beee0325fbbe4d4ce812a669693b7874835c8185a040a1b06628d70384d231302efac09872cf26328e5c7575deba8ffff19428ff75304c1eb918208c8ef38814620497c5a2eac195f08a4d5c9c1fa73d19bd8517ce5a57d61614fa2e3e88b17b880ba26070cddb36c1ecb1da62ad80272fb789613de2b4a51bb6f092c536315b1aa3bd3a2ddfaf4ca1d432584d1e9642020bd60ef170e51ccb344a39efaee4afb189dd1f466b87ae17f0df4d1ea1e24141adc7297db30cfc13390944bb92bfda26720e8ed02e666ee5665df96b6de2ab8c515aeee202ee4f6e7357e658d834e64961087a0a4fc09d98ecdbc194c2e65d713672e5c0db8395544c2006a9066729ef40a0f4a5f5164fface1bf6a2da25321fe97bcd27ee9dd896146d80f69c7b1b2ff87c69fbe3fe5a80713f198f6586d2fe4a54e303e564a44d8cb5ef73459d29e1c36378fba1d404088536be63a054fb40d1a6dbc4cc096fbac6ee2fe58b967e4b7b066a3d581f0d2e5335aa0e100a4a400741a9f2819239c626ca162d74dd3e7c803b1a6b7fa5574cd5a32a40281de391851b237beb1d1b1b431d3b7db32cc26f466993c2b673f285416d7aa422b6a9637b604c46c75eb89145f6f6a43b0490d171c74152c174a7c01a02f64dfbfcbf7da940959281764f0c520d6e4e7baf1e6fea3777eead615f4be68a9859238b07d7876a87ba14f38f1795b6e9193275163c249424a3226eba2e8b1fc470edddbb45dcac6b9e5b16ddbadab59bfd9cdc9fec2f6843e950b3358c7d2d79b60802c8d82215122d1c086eed5073ae787902cd4dc62462906ab069fd23079af5e11382cc9e1e9e2b8b4bd9a1daaecb9ff22f321f60c389daf4b41a3639c998862bce66561dc6fa703621c8030302284b198a3b714f7ca2224d789401f078e78796ae34a79771465b829643811cbaa700902a94bed87762c67626e21b7096423bf326e6913ef2b68ccfc7afd426de7f752097dc977cf270cd55595cb8bad795b36c1fdfbf6b720526831e03ab2980edcde7b44b01e66108d4774c2e7e9477a5afd9436bde96c55bc76768690b8b78d4f81e2a01790c948a585f0f512e7e94c7aef4f5780802284b994d1e017befb4e9baa72004cd87a7ac0fa51eb4f5f094199ed753df639c98b12a150fa57b8ace0fbf98f492b7682c7ac9f389efaffc38516a58e1d83a570aabebf4ec074fcbe2bdfa61dac999353cc15e518655cc32b06eb4658583e95306610002ae59ca1f708b624ad9b7ebd6d02333dfc5186208229606961a321d851e1b8a2c524f8dd7467c57cc1b2a73f5a5ba8d4c07d3a7900d08b81a36be79ba9bf2943bc5b0e1b318934c369639b61b36529364b1bb6e8a767fd6e72d712c73307d0ad980d064c38cc377714b1b0e47916123fe0959aceb3d5e5b9550c3d7ef3cdcbc570e07d3a7900d084d367a154c2ccb2f35f6ca77710b1beeaa88832d8a42765f59bcf8c938ad7a750dbf97be28b0a2d05f25a53a351585aa0876d05b142080b294a35d19231e834fdea240e1750ddfdfc4d39b0bc11d4c20666c3767ec3bb051085ec618e9a0d08ac1b7c9d51bb8f6cf52148205a195c6bb2746a993d5af96c5f1dc24edce9f8bf8e98363540c83da2c04ad67cf825056e97df78400ca523ec5955e38bc777aad30205958e1f096d943efa2de3a77fb9714bdcef5000462d43c159744d249bb9400fd5ebd0491c66a1bba24be70680ab2e72547ae27227e2b8bf85e43f5c85ebfe05a393d951ed92f39983e65100620e06a4476c4e119e5f55b06e8911defb318939a23db5d1a9d11a21721e8a46dd466c8e2f75343b4987545fc4f21396d764f7a0cfdad68ed1c52aa132242d2db446934cb08f1142108a02ca55f77c688e90c4f659d41c324b4093b8a90b755048758ba511161d88ed60ad122ec3b31639153991a3b95f4eec4d6925edac231d099fd1902a1f79924c95de8391c90251dde5f64713021545bf18f4f785cf1d9c0e0f9b40b1d5ee1ffb39957ce0d3e66ee14a9fd643164cb60ed65d9c9a3f27e0c2c6d5b3ac9e8e57383d931e67112045096b21941481c8e4f468cb7e124a4b339debddd2209e59918f19ec8d85db011ef3fc3b3a49b1c45e91071be315119371ae95c7ac439ca0c714a9c2cbeb29e77555d28e6db176e5049d763d2193f25a9fa2cba3455a6d223cc39ca031102c848674a65731a53ee99e3cd9bbaabb186d89cba946894baa4e391b4adac09ca332349ed238bbcd7aa5de1614efed6a414156b406d16a6d60f5e35412ccfeb2c08a0d690a083d3c12bf8a1d4951ce0c74127e15962332aca3affa8deb0cc36a33a374e162be79fd651cd2afbab4aaada85ea0d0b9bed450501d43aaaee5e54f093bf434a251011b740a5fa471574eabd7f3d2b59ea3acfeb932a0be78646d79de7b7f33b917b543857af1752c440a354d14438bb6e1d69c723d224f3a2381ab6e02f8449145692fb86cf49b5fe39599c69dce4bac1f6f3e589312a2d78ec8e35951e40f2fe387a23c093aa399adbfd7344c32ac7d1fdfa11ce19248b9cbc18ede41e5b52d7949efa16a1092996550ea64f1904381070cd5236a01a8ffdaf4371ce3b4b21c57c176388bd0ea6ad4a034b0d2b1d1f1acd2ef98455c4c747eacdae647abcd7ce68cb4a07d3a7901d08b81acd2e7dd65aa54f468c6ec77731263537bb86a58e6d868dfc5aabd8d66fb07ec4aa1d77bfb9a12e75307d0ad980d064e3d8a494f2678675d71baaefe29636d21c39868d924b5651bf3142c771aea2a6d9469a83e953c80684261b7f4f78a6fcf6dd41ba0ddfc52d6cb86f7942cbfb23e02bfbace237d9b5aebd5f14f35e25ebdbec6aa4a9109521d278d16694f5d9c7ac2275ce37ae693bb6f3d42fada27a6295eb035ecce3f75a457ad40faef3fff12f9377f5434053a8da9b43f55680a14ae7830e095777cee0a3c1e63a7523146fed7b6fb9e2429dfca71746ab30046a47f5cd19cc8e067bfa080450fbebd4bf51a38803ad53526572709919d5b91059fcb8e3aa2bfc61318f4c7b5d25558f45d55a774c0ea6f3a7f18f163a88fa43e5afe29e000ae28091b86ffac46ec969ab70beb3c7d5386f373f949adbe65986ec0c41c02d31c56efa21ab78d079a7ebb5a25ddcacabfeaa55ccfc799febfde0cfb83ff9d1d83595d99b01c66e47c4edff00449cbe802d4c4ee700e369504c003e29d5f40729b3180729b3184403000000010129b31800000000000002000729b3185dd1f88e0095f2a600b300000078016364000176cdcd129a601632c182cc1924ec77820c00c987022973e6687cfa03000011570b5b0f000000e505002b010030f1:26280/unsigned-big-integer>>,
	Size = size(Payload),
	io:format("payload func val: ~p~n size: ~p~n", [Payload, Size]),
	Payload.



get_byte_value(IndexName, Values, Offset) ->
	get_value(IndexName, Values, 1, Offset).

get_uint16_value(IndexName, Values, Offset) ->
	get_value(IndexName, Values, 2, Offset).

get_uint32_value(IndexName, Values) ->
	get_value(IndexName, Values, 4, 0).

get_uint64_value(IndexName, Values) ->
	get_value(IndexName, Values, 8, 0).

get_float_value(IndexName, Values) ->
	get_value(IndexName, Values, float).

get_value(IndexName, Values, float) ->
	% each Index is a 4 byte long word
	Index = update_fields:fields(IndexName) * 4,
	<<_Head:Index/binary, Value?f, _Tail/binary>> = Values,
	Value.
get_value(IndexName, Values, Size, Offset) ->
	% each Index is a 4 byte long word
	Index = (update_fields:fields(IndexName) * 4) + Offset,
	BitSize = Size*8,
	<<_Head:Index/binary, Value:BitSize/unsigned-little-integer, _Tail/binary>> = Values,
	Value.


set_byte_value(IndexName, Value, Values, Offset) ->
	set_value(IndexName, Value, Values, 1, Offset).

set_uint32_value(IndexName, Value, Values) ->
	set_value(IndexName, Value, Values, 4, 0).

set_uint16_value(IndexName, Value, Values, Offset) ->
	set_value(IndexName, Value, Values, 2, Offset).

set_float_value(IndexName, Value, Values, Offset) ->
	set_value(IndexName, Value, Values, float, Offset).

set_float_value(IndexName, Value, Values) ->
	set_value(IndexName, Value, Values, float).

set_uint64_value(IndexName, Value, Values) ->
	set_value(IndexName, Value, Values, 8, 0).

set_value(IndexName, Value, Values, float) ->
	set_value(IndexName, Value, Values, float, 0).

set_value(IndexName, Value, Values, float, Offset) ->
	% each Index is a 4 byte long word
	Index = (update_fields:fields(IndexName) + Offset) * 4,
	<<Head:Index/binary, _OldValue:4/binary, Tail/binary>> = Values,
	<<Head:Index/binary, Value?f, Tail/binary>>;
set_value(IndexName, Value, Values, Size, Offset) ->
	% each Index is a 4 byte long word
	Index = (update_fields:fields(IndexName) * 4) + Offset,
	BitSize = Size * 8,
	<<Head:Index/binary, _OldValue:Size/binary, Tail/binary>> = Values,
	<<Head:Index/binary, Value:BitSize/unsigned-little-integer, Tail/binary>>.
