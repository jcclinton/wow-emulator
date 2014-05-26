-module(opcode_patterns).

-export([getCallbackByNum/1, getNumByAtom/1]).


getCallbackByNum(16#0036) -> {character, create};
getCallbackByNum(16#0037) -> {character, enum};
getCallbackByNum(16#003D) -> {character, login};
getCallbackByNum(16#01DC) -> {server, pong};
getCallbackByNum(Unk) ->
	io:format("unknown opcode: ~p~n", [Unk]),
	{server, null}.


getNumByAtom(msg_null_action) -> 16#000;
getNumByAtom(smsg_char_enum) -> 16#03B;
getNumByAtom(smsg_char_create) -> 16#03A;
getNumByAtom(smsg_pong) -> 16#1DD;
getNumByAtom(smsg_login_verify_world) -> 16#236;
getNumByAtom(smsg_account_data_times) -> 16#209;
getNumByAtom(smsg_set_rest_start) -> 16#21E;
getNumByAtom(smsg_bindpointupdate) -> 16#155;
getNumByAtom(smsg_initial_spells) -> 16#12A;
getNumByAtom(smsg_send_unlearn_spells) -> 16#41D;
getNumByAtom(smsg_action_buttons) -> 16#129;
getNumByAtom(smsg_initialize_factions) -> 16#122;
getNumByAtom(smsg_login_settimespeed) -> 16#042;
getNumByAtom(smsg_init_world_state) -> 16#2C2;
getNumByAtom(smsg_tutorial_flags) -> 16#0FD;
getNumByAtom(smsg_update_object) -> 16#0A9;
getNumByAtom(Unk) ->
	io:format("unknown opcode: ~p~n", [Unk]),
	0.
