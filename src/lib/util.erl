-module(util).

-export([game_time/0, game_speed/0]).
-export([call/4]).


game_speed() ->
	0.01666667.

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


% used to call callback functions
% if a callback returns ok, nothing happens
% if it returns {OpAtom, Payload}
% it sends that packet to this player
call(M, F, Args, AccountId) ->
	Data = recv_data:build(Args),
	try M:F(Data) of
		ok -> ok;
		{OpAtom, Payload} ->
			player_router:send(AccountId, OpAtom, Payload)
		catch
			Error ->
				io:format("error in char: ~p~n", [Error]),
				ok
		end.
