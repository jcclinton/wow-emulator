-module(player_worker).

-export([start_link/1, call/1]).


start_link(WorkerData) ->
	spawn_link(?MODULE, call, [WorkerData]).



% used to call callback functions
% if a callback returns ok, nothing happens
% if it returns {OpAtom, Payload}
% it sends that packet to this player
call({M, F, Args}) ->
	Data = recv_data:build(Args),
	case M:F(Data) of
		ok -> ok;
		{OpAtom, Payload} ->
			AccountId = recv_data:get(account_id, Data),
			player_controller:send(AccountId, OpAtom, Payload)
	end.
