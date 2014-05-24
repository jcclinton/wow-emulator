-module(character).
-export([enum/1, create/1]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/database_records.hrl").

enum(PropList) ->
	PlayerName = proplists:get_value(account_id, PropList),
	Chars = ets:match(characters, {'_', PlayerName, '_'}),
	io:format("matched: ~p~n", [Chars]),
	Pids = [self()],
	Opcode = opcode_patterns:getNumByAtom(smsg_char_enum),
	Num = 0,
	Msg = <<Opcode?W, Num?B>>,
	{[], {Pids, Msg}}.


create(PropList) ->
	Pids = [self()],
	Payload = proplists:get_value(payload, PropList),
	PlayerName = proplists:get_value(account_id, PropList),
	{Name, NewPayload} = extract_name(Payload),
	<<Race?B, Class?B, Gender?B, Skin?B, Face?B, HairStyle?B, HairColor?B, FacialHair?B, OutfitId?B>> = NewPayload,
	Char = #char{name=Name, race=Race, class=Class, gender=Gender, skin=Skin, face=Face, hair_style=HairStyle, hair_color=HairColor, facial_hair=FacialHair, outfit_id=OutfitId},
	ets:insert(characters, {Name, PlayerName, Char}),
	Opcode = opcode_patterns:getNumByAtom(smsg_char_create),
	Result = 16#2E,
	Msg = <<Opcode?W, Result?B>>,
	{[], {Pids, Msg}}.




%%%%%%%%%%%%
%% private

extract_name(Payload) ->
	extract_name(Payload, []).
extract_name(<<0?B, Rest/binary>>, Name) ->
	NameBin = list_to_binary(lists:reverse(Name)),
	{NameBin, Rest};
extract_name(<<Char?B, Rest/binary>>, Name) ->
	extract_name(Rest, [Char|Name]).
	
