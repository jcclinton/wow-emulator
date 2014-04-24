-module(movement).

-export([]).





forward(User) ->
	UserIds = bsp_tree:getNearbyUsers(User#user.Pos),
	Pids = PidLib:getById(UserIds),
	Msg = {forward, some_data},
	%TODO store data back in user state
	%NewPos = UserPos + 1,
	%NewUser = update_user(User, NewPos),
	%% Msg should be in the format <<Opcode?WO, Payload/binary>>
	Messages = {Pids, Msg},
	{NewUser, Messages}.
