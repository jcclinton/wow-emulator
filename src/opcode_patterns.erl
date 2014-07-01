-module(opcode_patterns).

-export([getCallbackByNum/1, getNumByAtom/1]).


getCallbackByNum(16#0036) -> {character, create};
getCallbackByNum(16#0037) -> {character, enum};
getCallbackByNum(16#003D) -> {character, login};
getCallbackByNum(16#00B5) -> {movement, handle_movement};
getCallbackByNum(16#00B6) -> {movement, handle_movement};
getCallbackByNum(16#00B7) -> {movement, handle_movement};
getCallbackByNum(16#00BC) -> {movement, handle_movement};
getCallbackByNum(16#00BD) -> {movement, handle_movement};
getCallbackByNum(16#00BE) -> {movement, handle_movement};
getCallbackByNum(16#00EE) -> {movement, handle_movement};
getCallbackByNum(16#01DC) -> {server, pong};
getCallbackByNum(16#01ED) -> {server, accept_challenge};
getCallbackByNum(Unk) ->
	io:format("unknown opcode by num: ~p~n", [Unk]),
	{server, null}.


getNumByAtom(msg_null_action) -> 16#000;
getNumByAtom(msg_move_start_forward) -> 16#0B5;
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
getNumByAtom(smsg_init_world_states) -> 16#2C2;
getNumByAtom(smsg_tutorial_flags) -> 16#0FD;
getNumByAtom(smsg_update_object) -> 16#0A9;
getNumByAtom(smsg_compressed_update_object) -> 16#1F6;
getNumByAtom(smsg_messagechat) -> 16#096;
getNumByAtom(smsg_auth_challenge) -> 16#1EC;
getNumByAtom(cmsg_challenge_accept) -> 16#1ED;
getNumByAtom(smsg_challenge_accept) -> 16#1EE;
getNumByAtom(Unk) ->
	io:format("unknown opcode by atom: ~p~n", [Unk]),
	0.
