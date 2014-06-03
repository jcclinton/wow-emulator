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
	Guid = world:get_guid(),
	PropList2 = [{guid, Guid} | PropList],
	Char = create_char_record(PropList2),
	Values = create_char_values(PropList2, Char),
	%io:format("storing char name: ~p under player name: ~p~n", [Name, PlayerName]),
	ets:insert(characters, {Char#char.name, Char#char.account_id, Char#char.id, Char, Values}),
	Opcode = opcode_patterns:getNumByAtom(smsg_char_create),
	Result = 16#2E,
	Msg = <<Opcode?W, Result?B>>,
	world_socket_controller:send(Msg),
	ok.

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
	
	Unk3 = 16#08,
	Unk5 = 16#20,
	ModelId = Char#char.model_id + Gender,
	NativeModelId = Char#char.model_id + Gender,
	%Guid = <<7,41,179,24>>,
	%PackedGuidBin = <<7, 41,179,24>>,
	%<<PackedGuid?LB>> = PackedGuidBin,
		PackedGuidBin = <<41,179,24, 0>>,
	<<PackedGuid?L>> = PackedGuidBin,
	KeyValues = [
		{'OBJECT_FIELD_GUID', PackedGuid, uint64},
		{'OBJECT_FIELD_TYPE', 25, uint32},
		{'UNIT_FIELD_BYTES_0', Race, byte_0},
		{'UNIT_FIELD_BYTES_0', Class, byte_1},
		{'UNIT_FIELD_BYTES_0', Gender, byte_2},
    {'UNIT_FIELD_BYTES_2', Unk3 bor Unk5, byte_1},
    {'UNIT_FIELD_LEVEL', 1, uint32},
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
	%io:format("indexname: ~p~nvalue: ~p~n", [IndexName, Value]),
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


                 %general_flags    = 16#10A00040,


login(PropList) ->
	_PlayerName = proplists:get_value(account_id, PropList),
	<<Guid?Q>> = proplists:get_value(payload, PropList),
	[{_,_,Guid,Char, Values}] = ets:match_object(characters, {'_', '_', Guid, '_', '_'}),
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
	PropList3 = [{values, Values}|PropList2],
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
	update_object(PropList3),
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
	Msg = <<Opcode?W, Payload/binary>>,
	world_socket_controller:send(Msg),
	ok.

	testPayload() ->
	Char = {char,1,"ALICE",1,<<"Serinthely">>,human,male,warrior,rage,3,3,8,8,1,
            undefined,1,0,278921280,0,1,0,12,-8949.95,-132.493,83.5312,0,49,
            24.0,16.200000000000003,22.0,4.0,6.0,60,undefined,undefined,100,
            81,undefined,5,7,1},
		Values = get_hardcoded_value(),
		Block = block(Char, Values),
			io:format("block: ~p~n", [Block]),
		ok.

	countBits() ->
		MaskBits_real = <<21,84,209,249,24,64,0,0,0,0,0,0,0,0,0,192,25,4,192,15,28,0,0,16,6,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,128,63,0,0,
            0,0,32,0,0,0,0,0,0>>,
		MaskBits_recorded = <<21,0,64,84,29,192,0,0,0,0,0,128,32,0,0,192,223,4,194,15,56,25,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,16,0,1,0,0,0,0,0,0,0,0,0,0,0,1,16,0,0,0,0,48,60,0,240,0,240,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,224,190,111,219,54,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,108,4,0,0,0,0,0,0,128,0,0,0,0,128,127,0,0,0,0,32,0,0,0,0,0,0>>,
		ValuesDataRecorded = <<41,179,24,0,25,0,0,0,0,0,128,63,60,0,0,0,100,0,0,0,60,0,0,0,232,3,0,0,100,0,0,0,1,0,0,0,1,0,0,0,1,1,0,1,8,0,0,0,153,9,0,0,9,0,0,0,1,0,0,0,108,7,0,0,208,7,0,0,208,7,0,0,8,172,156,62,0,0,192,63,49,0,0,0,49,0,0,0,95,241,157,64,95,241,221,64,1,238,17,0,0,0,128,63,23,0,0,0,20,0,0,0,22,0,0,0,20,0,0,0,22,0,0,0,47,0,0,0,20,0,0,0,0,40,0,0,29,0,0,0,11,0,0,0,73,146,36,64,73,146,100,64,7,10,3,3,4,0,0,1,38,0,0,0,39,0,0,0,40,0,0,0,25,0,0,0,58,9,0,0,70,120,11,84,0,0,0,64,72,120,11,84,0,0,0,64,74,120,11,84,0,0,0,64,76,120,11,84,0,0,0,64,78,120,11,84,0,0,0,64,80,120,11,84,0,0,0,64,82,120,11,84,0,0,0,64,144,1,0,0,26,0,0,0,1,0,1,0,43,0,0,0,1,0,5,0,0,0,5,0,44,0,0,0,1,0,5,0,54,0,0,0,1,0,5,0,0,0,5,0,95,0,0,0,1,0,5,0,98,0,0,0,44,1,44,1,162,0,0,0,1,0,5,0,157,1,0,0,1,0,1,0,158,1,0,0,1,0,1,0,159,1,0,0,1,0,1,0,177,1,0,0,1,0,1,0,2,0,0,0,72,225,154,64,144,111,151,64,246,213,157,64,226,39,150,64,0,0,0,32,37,0,0,0,0,0,128,63,0,0,128,63,0,0,128,63,0,0,128,63,0,0,128,63,0,0,128,63,0,0,128,63,0,0,15,0,255,255,255,255>>,
		ValuesDataReal = <<41,179,24,0,25,0,0,0,0,0,128,63,
              1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,120,0,0,0,100,0,0,0,100,
              0,0,0,100,0,0,0,120,0,0,0,100,0,0,0,100,0,0,0,100,0,0,0,35,0,0,0,
              1,1,0,0,8,0,0,0,0,0,250,68,0,0,250,68,0,0,250,68,1,0,0,0,1,0,0,0,
              0,238,0,0,10,0,0,0,10,0,0,0,10,0,0,0,10,0,0,0,10,0,0,0,10,0,0,0,
              10,0,0,0,10,0,0,0,0,40,0,0,1,0,0,0,7,3,6,1,7,0,0,2,1,0,0,0,10,0,
              0,0,10,0,0,0,0,0,128,63,0,0,128,63,0,0,128,63,0,0,128,63,0,0,128,
              63,0,0,128,63,0,0,128,63,255,255,0,0>>,
		TotalCount = update_fields:fields('PLAYER_END'),
		MaskBitsRealSize = lists:foldl(fun(Index, {Num, Mask})->
			Bool = getBit(Mask, Index),
				if Bool ->
					{Num+1, Mask};
				true -> {Num, Mask}
				end
			end, {0, MaskBits_real}, lists:seq(0, TotalCount-1)),
		MaskBitsRecSize = lists:foldl(fun(Index, {Num, Mask})->
			Bool = getBit(Mask, Index),
				if Bool ->
					{Num+1, Mask};
				true -> {Num, Mask}
				end
			end, {0, MaskBits_real}, lists:seq(0, TotalCount-1)),

			io:format("real: ~n"),
			lists:foldl(fun(Index, {Offset, Mask}) ->
				Bool = getBit(Mask, Index),
					if Bool ->
						Value = get_value(Index-Offset, ValuesDataReal),
						io:format("~p: ~p~n", [Index, Value]),
						{Offset, Mask};
					true -> {Offset+1, Mask}
					end
			end, {0, MaskBits_real}, lists:seq(0, TotalCount-1)),

			io:format("recorded: ~n"),
			lists:foldl(fun(Index, {Offset, Mask}) ->
				Bool = getBit(Mask, Index),
					if Bool ->
						Value = get_value(Index-Offset, ValuesDataRecorded),
						io:format("~p: ~p~n", [Index, Value]),
						{Offset, Mask};
					true -> {Offset+1, Mask}
					end
			end, {0, MaskBits_recorded}, lists:seq(0, TotalCount-1)),

			RealValueSize = byte_size(ValuesDataReal),
			RecValueSize = byte_size(ValuesDataRecorded),
			io:format("maskbits real: ~p~nreal value size: ~p~nmaskbits rec: ~p~n rec value size: ~p~n", [MaskBitsRealSize, RealValueSize, MaskBitsRecSize, RecValueSize]),
			ok.
			

update_object(Proplist) ->
	Opcode = opcode_patterns:getNumByAtom(smsg_update_object),
	CompressedOpcode = opcode_patterns:getNumByAtom(smsg_compressed_update_object),
	Char = proplists:get_value(char, Proplist),
	Values = proplists:get_value(values, Proplist),
	
	Block = block(Char, Values),

	BlockCount = 1,
	HasTransport = 0,
	Payload = <<BlockCount?L, HasTransport?B, Block/binary>>,
	Msg = if byte_size(Payload) > 100 ->
			CompressedPayload = compress(Payload),
			%io:format("payload: ~p~n", [Payload]),
			Size = byte_size(Payload),
			<<CompressedOpcode?W, Size?L, CompressedPayload/binary>>;
		true ->
			<<Opcode?W, Payload/binary>>
		end,
	world_socket_controller:send(Msg),
	ok.

decompress(Packet) ->
	Z = zlib:open(),
	zlib:inflateInit(Z),
	Data = zlib:inflate(Z, Packet),
	zlib:inflateEnd(Z),
	list_to_binary(Data).

compress(Packet) ->
    Z  = zlib:open(),
    ok = zlib:deflateInit(Z, best_speed),
    P  = zlib:deflate(Z, Packet, finish),
    %L  = zlib:deflate(Z, [], finish),
    ok = zlib:deflateEnd(Z),
    %zlib:close(Z),
    %list_to_binary([P|L]).
    list_to_binary(P).

	block(Char, Values) ->
		UpdateType = 3, %char_create2
		%Guid = Char#char.id,
		Guid = <<7,41,179,24>>,
		TypeId = 4, %type player
		MovementData = getMovementData(Char),
		ValuesCount = (byte_size(Values) div 4) - 1,
		EmptyMaskBits = create_mask(ValuesCount),
		MaskBits = set_create_bits(ValuesCount, EmptyMaskBits, Values),
		io:format("maskbits: ~p~n", [MaskBits]),
		ValuesData = build_values_update(MaskBits, Values, ValuesCount),
		io:format("value bits: ~p~n", [ValuesData]),

		<<UpdateType?B, Guid/binary, TypeId?B, MovementData/binary, ValuesData/binary>>.


	build_values_update(MaskBits, Values, Count) ->
		Blocks = (Count + 31) div 32,
		ValuesData = lists:foldl(fun(Index, Bin) ->
			BitFlag = getBit(MaskBits, Index),
			if BitFlag ->
								Value = get_value(Index, Values),
								<<Bin/binary, Value?L>>;
							true -> Bin
						end
		end, <<>>, lists:seq(0, Count)),
		<<Blocks?B, MaskBits/binary, ValuesData/binary>>.


	get_value(Index, Values) ->
		get_uint32_value(Index, Values).


	getBit(Mask, Index) ->
				MaskIndex = Index bsr 3,
				LowIndex = Index band 16#7,
				Value = 1 bsl LowIndex,
				ByteSize = MaskIndex,
				<<_Head:ByteSize/binary, OldValue?B, _Tail/binary>> = Mask,
				NewValue = OldValue band Value,
				NewValue > 0.


	create_mask(Count) ->
		Blocks = (Count + 31) div 32,
		binary:copy(<<0?L>>, Blocks).


	set_create_bits(Count, EmptyMask, Values) ->
		lists:foldl(fun(Index, Mask) ->
			Value = get_value(Index, Values),
			Bit = if Value > 0 -> 1;
							true -> 0
						end,
			MaskIndex = Index bsr 3,
			LowIndex = Index band 16#7,
			BitValue = Bit bsl LowIndex,
			ByteSize = MaskIndex,
			<<Head:ByteSize/binary, OldValue?B, Tail/binary>> = Mask,
			NewValue = OldValue bor BitValue,
			<<Head/binary, NewValue?B, Tail/binary>>
		end, EmptyMask, lists:seq(0, Count)).


	getMovementData(Char) ->
		All = 16#10,
		Self = 16#01,
		Living = 16#20,
		HasPosition = 16#40,
		UpdateFlags = All bor Self bor Living bor HasPosition,
		MoveFlags = 0,
		WorldTime = 1000,
        Speeds         = {2.5, 7, 4.5, 4.72, 2.5,
                          7, 4.5, 3.141593, 1.0},
    {W, R, WB, S, SB, _F, _FB, T, _P} = Speeds,
		X = Char#char.position_x,
		Y = Char#char.position_y,
		Z = Char#char.position_z,
		O = Char#char.orientation,
		<<UpdateFlags?B, MoveFlags?L, WorldTime?L, X?f, Y?f, Z?f, O?f, 0?f, W?f, R?f, WB?f, S?f, SB?f, T?f, 1?L>>.




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
get_value(IndexName, Values, Size, Offset) when is_atom(IndexName) ->
	% each Index is a 4 byte long word
	Index = update_fields:fields(IndexName),
	get_value(Index, Values, Size, Offset);
get_value(IndexIn, Values, Size, Offset) ->
	Index = (IndexIn * 4) + Offset,
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


get_hardcoded_value() ->
<<41,179,24,0,0,0,0,0,25,0,0,0,0,0,0,0,0,0,128,63,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,120,0,0,0,100,0,0,0,100,0,
         0,0,0,0,0,0,0,0,0,0,100,0,0,0,120,0,0,0,100,0,0,0,100,0,0,0,100,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,35,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,250,68,0,0,
         250,68,0,0,250,68,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,238,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,10,0,0,0,10,
         0,0,0,10,0,0,0,10,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,10,0,0,0,10,0,0,0,0,40,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,8,8,1,0,0,2,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,63,0,0,128,63,0,0,
         128,63,0,0,128,63,0,0,128,63,0,0,128,63,0,0,128,63,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>.
