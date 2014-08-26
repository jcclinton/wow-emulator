-module(character).
-export([enum/1, create/1, delete/1, logout/1, login/1, update_account_data/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").
-include("include/types.hrl").
-include("include/character.hrl").
-include("include/unit.hrl").
-include("include/object.hrl").
-include("include/shared_defines.hrl").



update_account_data(_Data) ->
	% dont need to do anything
	ok.

enum(Data) ->
	AccountId = recv_data:get(account_id, Data),
	{Num, CharDataOut} = player_model_controller:enum(AccountId),
	Msg = <<Num?B, CharDataOut/binary>>,
	{smsg_char_enum, Msg}.


delete(Data) ->
	AccountId = recv_data:get(account_id, Data),
	<<Guid?Q>> = recv_data:get(payload, Data),

	Result = player_model_controller:delete(AccountId, Guid),
	Msg = <<Result?B>>,
	{smsg_char_delete, Msg}.


create(Data) ->
	Payload = recv_data:get(payload, Data),
	AccountId = recv_data:get(account_id, Data),

	Result = player_model_controller:create(AccountId, Payload),

	%Result = 16#2E, % success
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

	% after a few seconds, kill player controller
	% this will give the client time to send any shutdown commands
	timer:apply_after(1000, player_controller, logout_char, [AccountId]),
	timer:apply_after(1000, player_model_controller, logout, [AccountId]),
	ok.


login(Data) ->
	AccountId = recv_data:get(account_id, Data),
	<<Guid?Q>> = recv_data:get(payload, Data),
	player_controller:login_char(AccountId, Guid),
	player_model_controller:login(AccountId, Guid),



	Char = char_data:get_char_move(Guid),
	%io:format("logging in ~p~n", [CharName]),
	X = Char#char_move.x,
	Y = Char#char_move.y,
	Z = Char#char_move.z,
	MapId = Char#char_move.map,
	Orientation = Char#char_move.orient,
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
	Char = char_data:get_char_move(Guid),
	Zone = Char#char_move.zone,
	Map = Char#char_move.map,
	X = Char#char_move.x,
	Y = Char#char_move.y,
	Z = Char#char_move.z,
	Payload = <<X?f, Y?f, Z?f, Map?L, Zone?L>>,
	{smsg_bindpointupdate, Payload}.

initial_spells(Data) ->
	Guid = recv_data:get(guid, Data),
	Record = char_data:get_char_spells(Guid),
	SpellIds = Record#char_spells.ids,
	NumSpells = length(SpellIds),
	Spells = lists:foldl(fun(Id, Acc) ->
		<<Acc/binary, Id?W, 0?W>>
	end, <<>>, SpellIds),
	NumSpellsOnCooldown = 0,
	Payload = <<0?B, NumSpells?W, Spells/binary, NumSpellsOnCooldown?W>>,
	{smsg_initial_spells, Payload}.

%send_unlearn_spells(_Data) ->
	%Payload = <<0?L>>,
	%{smsg_send_unlearn_spells, Payload}.

action_buttons(Data) ->
	Guid = recv_data:get(guid, Data),
	Payload = char_data:get_action_buttons(Guid),
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
	Char = char_data:get_char_move(Guid),
	MapId = Char#char_move.map,
	ZoneId = Char#char_move.zone,
	Count = 6,
	%Payload = <<MapId?L, ZoneId?L, Count?W, 16#8d8?L, 0?L, 16#8d7?L, 0?L, 16#8d6?L, 0?L, 16#8d5?L, 0?L, 16#8d4?L, 0?L, 16#8d3?L, 0?L>>,
	Rest = <<16#d808000000000000d708000000000000d608000000000000d508000000000000d408000000000000d308000000000000:384/unsigned-big-integer>>,
	Payload = <<MapId?L, ZoneId?L, Count?W, Rest/binary>>,
	{smsg_init_world_states, Payload}.
