-module(player_account).
-behavior(gen_server).

-record(state, {
	account_id
}).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_pid/1, send/3, handle_packet/5]).


-include("include/binary.hrl").


%% public api

send(Name, OpAtom, Payload) ->
	Pid = player_router:get_pid(Name),
	player_router:send(Pid, OpAtom, Payload).

get_pid(AccountId) ->
	world:get_pid(AccountId ++ "_account").


handle_packet(AccountId, OpAtom, M, F, Payload) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {packet_rcvd, OpAtom, M, F, Payload}).



%% behavior callbacks

start_link(AccountId) ->
	Pid = get_pid(AccountId),
	gen_server:start_link(Pid, ?MODULE, {AccountId}, []).

init({AccountId}) ->
	io:format("account SERVER: started~n"),
	{ok, #state{account_id=AccountId}}.

handle_cast({packet_rcvd, OpAtom, M, F, Payload}, State = #state{account_id=AccountId}) ->
	Args = [{payload, Payload}, {op_atom, OpAtom}],
	try M:F(Args, AccountId) of
		_ -> ok
		catch
			Error -> io:format("error in account: ~p~n", [Error]),
				ok
		end,
	{noreply, State};
handle_cast(Msg, State) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, State}.


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, _State) ->
	io:format("WORLD: shutting down account~n"),
	ok.
