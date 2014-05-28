-module(character).
-export([enum/1, create/1, login/1]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/database_records.hrl").
-include("include/update.hrl").

enum(PropList) ->
	PlayerName = proplists:get_value(account_id, PropList),
	Chars = ets:match_object(characters, {'_', PlayerName, '_', '_'}),
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
	Char = create_char(PropList),
	%io:format("storing char name: ~p under player name: ~p~n", [Name, PlayerName]),
	ets:insert(characters, {Char#char.name, Char#char.account_id, Char#char.id, Char}),
	Opcode = opcode_patterns:getNumByAtom(smsg_char_create),
	Result = 16#2E,
	Msg = <<Opcode?W, Result?B>>,
	world_socket_controller:send(Msg),
	ok.

create_char(PropList) ->
	Payload = proplists:get_value(payload, PropList),
	PlayerName = proplists:get_value(account_id, PropList),
	{Name, NewPayload} = extract_name(Payload),
    <<Race?B, Class?B, Gender?B, Skin?B,
      Face?B, HS?B, HC?B, FH?B, _?B>> = NewPayload,
    RaceName    = char_helper:to_race(Race),
    ClassName   = char_helper:to_class(Class),
    CreateInfo  = content:char_create_info(RaceName, ClassName),
		Guid = world:get_guid(),
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


login(PropList) ->
	_PlayerName = proplists:get_value(account_id, PropList),
	<<Guid?Q>> = proplists:get_value(payload, PropList),
	[{_,_,Guid,Char}] = ets:match_object(characters, {'_', '_', Guid, '_'}),
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
	set_rest_start(PropList2),
	set_tutorial_flags(PropList2),

	%bind_point_update(PropList2),

	initial_spells(PropList2),

	%send_unlearn_spells(PropList2),
	action_buttons(PropList2), % differs
	initialize_factions(PropList2), % differs
	login_settimespeed(PropList2),

	%login packets to send after player is added to map
	update_object(PropList2),
	init_world_state(PropList2),
	%update_object2(PropList2),
	%castspell
	%enchantment
	%item enchanctment
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
	GameTime = game_time(),
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
	Payload = <<MapId?L, ZoneId?L, Count?W, 16#8d8?L, 0?L, 16#8d7?L, 0?L, 16#8d6?L, 0?L, 16#8d5?L, 0?L, 16#8d4?L, 0?L, 16#8d3?L, 0?L>>,
	Size = 138 * 8,
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
	io:format("update payload: ~p~n", [Payload]),
	Msg = <<Opcode?W, BlockCount?L, HasTransport?B, Payload/binary>>,
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
	

mapCharData({_CharName, _AccountName, _, #char{id=Guid, name=Name, race=RaceName, class=ClassName, gender=GenderName, skin=Skin, face=Face, hair_style=HairStyle, hair_color=HairColor, facial_hair=FacialHair, level=Level, zone_id=Zone, map_id=Map, position_x=X, position_y=Y, position_z=Z, guild_id=GuildId, general_flags=GeneralFlags, at_login_flags=AtLoginFlags}}) ->
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
