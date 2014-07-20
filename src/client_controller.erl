-module(client_controller).
-behavior(gen_server).

-record(state, {
	account_id,
  send_pid
							 }).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([move/0]).
-compile([export_all]).


-include("include/binary.hrl").
-include("include/database_records.hrl").


move() ->
	AccountId = "ALICE",
	Pid = client_rcv:get_pid(AccountId),
	gen_server:cast(Pid, move).




	

start_link(AccountId, SendPid) ->
	Pid = client_rcv:get_pid(AccountId),
	gen_server:start_link(Pid, ?MODULE, {AccountId, SendPid}, []).

init({AccountId, SendPid}) ->
	io:format("controller SERVER: started~n"),
	process_flag(trap_exit, true),

	{ok, #state{send_pid=SendPid, account_id=AccountId}}.


handle_cast(move, State) ->
	Opcode = 16#0B5,
	Char = #char{ position_x = -8949.95, position_y = -132.493, position_z = 83.5312, orientation = 0},
	X = Char#char.position_x,
	Y = Char#char.position_y,
	Z = Char#char.position_z,
	O = Char#char.orientation,
	Time = util:game_time(),
	Unk1 = 0,
	MoveFlags = 1,
	Payload = <<MoveFlags?L, Time?L, X?f, Y?f, Z?f, O?f, Unk1?L>>,
	Msg = <<Opcode?L, Payload/binary>>,
	gen_server:cast(self(), {send_to_server, Msg}),
	{noreply, State};
handle_cast({tcp_packet_rcvd, <<Opcode?L, Payload/binary>>}, State) ->
	handle_response(Opcode, Payload),
	{noreply, State};
handle_cast({send_to_server, Msg}, S=#state{send_pid = SendPid}) ->
	gen_fsm:send_event(SendPid, {send, Msg}),
	{noreply, S};
handle_cast(Msg, S) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, S}.


handle_call(_E, _From, State) ->
	{reply, ok, State}.

handle_info(upgrade, State) ->
	%% loads latest code
	?MODULE:handle_info(do_upgrade, State),
	{noreply, State};
handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, _State) ->
	io:format("WORLD: shutting down controller~n"),
	ok.


%private

handle_response(Opcode, Payload) ->
		io:format("client looking up opcode: ~p~n", [Opcode]),
	Fun = lookup_opcode(Opcode),
	if Fun /= false ->
			case Fun(Payload) of
				false -> ok;
				{OutOpcode, Response} ->
					Msg = <<OutOpcode?L, Response/binary>>,
					gen_server:cast(self(), {send_to_server, Msg}),
					ok
			end;
		true ->
			ok
	end.

lookup_opcode(16#03B) -> fun player_login/1;
lookup_opcode(16#1EE) -> fun send_char_enum/1;
lookup_opcode(_) -> false.


player_login(Payload) ->
	EQUIPMENT_SLOT_END = 19,
	SlotDataSize = EQUIPMENT_SLOT_END * 40,
	<<Num?B, CharData/binary>> = Payload,
	<<Guid?Q,
	NameNum1?B,
	NameNum2?B,
	NameNum3?B,
	NameNum4?B,
	0?B,
	_Race?B,
	_Class?B,
	_Gender?B,
	_Skin?B,
	_Face?B,
	_HairStyle?B,
	_HairColor?B,
	_FacialHair?B,
	_Level?B,
	_Zone?L,
	_Map?L,
	_X?f,
	_Y?f,
	_Z?f,
	_GuildId?L,
	_GeneralFlags?L,
	_AtLoginFlags?B,
	_PetDisplayId?L,
	_PetLevel?L,
	_PetFamily?L,
	0:SlotDataSize/unsigned-little-integer,
	_BagDisplayId?L,
	_BagInventoryType?B>> = CharData,
	Name = [NameNum1, NameNum2, NameNum3, NameNum4],
	io:format("received enum with ~p chars. name: ~p guid: ~p~n", [Num, Name, Guid]),
	% send login
	Opcode = 16#003D,
	{Opcode, <<Guid?Q>>}.


send_char_enum(_) ->
	Opcode = 16#0037,
	{Opcode, <<0?L>>}.
		
