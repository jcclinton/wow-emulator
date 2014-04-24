-module(movement).

-export([]).





forward(User) ->
	UserIds = bsp_tree:getNearbyUsers(User#user.Pos),
	Pids = PidLib:getById(UserIds),
	Msg = {forward, some_data},
	sockserv_send:send(Pids, Msg),
	%TODO store data back in user state
	%NewPos = UserPos + 1,
	%NewUser = update_user(User, NewPos),
	NewUser.












%% from sockserv_send:send

sockserv_send([], Msg) -> ok;
sockserv_send([Pid|Rest], Msg) ->
	sockserv_send(Pid, Msg);
	sockserv_send(Rest, Msg);
sockserv_send(Pid, Msg) when is_pid(Pid) ->
	%% actual socket_send, with encryption
	ok.
