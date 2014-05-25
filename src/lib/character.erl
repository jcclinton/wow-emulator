-module(character).
-export([enum/1, create/1]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/database_records.hrl").

enum(PropList) ->
	PlayerName = proplists:get_value(account_id, PropList),
	Chars = ets:match_object(characters, {'_', PlayerName, '_'}),
	io:format("looking up player name: ~p~n", [PlayerName]),
	io:format("matched: ~p~n", [Chars]),
	Pids = [self()],
	Opcode = opcode_patterns:getNumByAtom(smsg_char_enum),
	Num = length(Chars),
	CharDataOut2 = if Num > 0 ->
								CharList = lists:map(fun mapCharData/1, Chars),
								CharData = iolist_to_binary(CharList),
	%io:format("mapped char data: ~p~n", [CharData]),
								CharData;
							true -> <<>>
						end,
	%1d 82 38 c9 01 ce
	Size = 170*8,
	CharDataOut = <<Opcode?W, 16#01ced314000000000046726f737465656675780005080002060802023c6a020000010000005c49d6450a4b91c57f3a2f440000000000000000000000000000000000000000006f7900000182260000020376000003000000000097730000149c7300000659720000079a730000089d73000009997300000ad65d00000b807b00000b282000000c107400000c2a89000010795c0000110000000000538800001a00000000000000000000:Size/unsigned-big-integer>>,
	%Msg = <<Opcode?W, CharDataOut/binary>>,
	Msg = CharDataOut,
	io:format("chardataout: ~p~n", [CharDataOut]),
	io:format("msg: ~p~n", [Msg]),
	{[], {Pids, Msg}}.


create(PropList) ->
	Pids = [self()],
	Payload = proplists:get_value(payload, PropList),
	PlayerName = proplists:get_value(account_id, PropList),
	{Name, NewPayload} = extract_name(Payload),
	<<Race?B, Class?B, Gender?B, Skin?B, Face?B, HairStyle?B, HairColor?B, FacialHair?B, OutfitId?B>> = NewPayload,
	Guid = 1,
	Char = #char{guid=Guid, name=Name, race=Race, class=Class, gender=Gender, skin=Skin, face=Face, hair_style=HairStyle, hair_color=HairColor, facial_hair=FacialHair, outfit_id=OutfitId},
	io:format("storing char name: ~p under player name: ~p~n", [Name, PlayerName]),
	ets:insert(characters, {Name, PlayerName, Char}),
	Opcode = opcode_patterns:getNumByAtom(smsg_char_create),
	Result = 16#2E,
	Msg = <<Opcode?W, Result?B>>,
	{[], {Pids, Msg}}.


login(PropList) ->
	Pids = [self()],
	PlayerName = proplists:get_value(account_id, PropList),
	Opcode = opcode_patterns:getNumByAtom(smsg_login_verify_world),
	MapId = 1,
	X = 1,
	Y = 1,
	Z = 1,
	Orient = 1,
	Payload = <<MapId?L, X?f, Y?f, Z?f, Orient?f>>,
	io:format("login payload: ~p~n", [Payload]),
	Msg = <<Opcode?W, Payload/binary>>,
	{[], {Pids, Msg}}.




%%%%%%%%%%%%
%% private

extract_name(Payload) ->
	extract_name(Payload, []).
extract_name(<<0?B, Rest/binary>>, Name) ->
	NameBin = iolist_to_binary(lists:reverse(Name)),
	{NameBin, Rest};
extract_name(<<Char?B, Rest/binary>>, Name) ->
	extract_name(Rest, [Char|Name]).
	

mapCharData({_CharName, _AccountName, #char{guid=Guid, name=Name, race=Race, class=Class, gender=Gender, skin=Skin, face=Face, hair_style=HairStyle, hair_color=HairColor, facial_hair=FacialHair}}) ->
	NameSize = size(Name) * 8,
	Level = 10,
	Zone = 1,
	Map = 1,
	X = 1,
	Y = 1,
	Z = 1,
	GuildId = 0,
	PlayerFlags = 0,
	AtLoginFlags = 0,
	PetDisplayId = 0,
	PetLevel = 0,
	PetFamily = 0,
	EQUIPMENT_SLOT_END = 19,
	SlotDataSize = EQUIPMENT_SLOT_END * 40,
	BagDisplayId = 0,
	BagInventoryType = 0,
	<<NameNum:NameSize/unsigned-big-integer>> = Name,
	<<Guid?L,
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
	Level?f,
	Zone?f,
	Map?f,
	X?f,
	Y?f,
	Z?f,
	GuildId?L,
	PlayerFlags?L,
	AtLoginFlags?B,
	PetDisplayId?L,
	PetLevel?L,
	PetFamily?L,
	0:SlotDataSize/unsigned-little-integer,
	BagDisplayId?L,
	BagInventoryType?B>>.

