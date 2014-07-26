-module(player_account).
-behavior(gen_server).

-record(state, {
	account_id
}).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_pid/1, send/3, handle_packet/4]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").


%% public api

send(Name, OpAtom, Payload) ->
	RouterPid = player_controller:get_pid(Name),
	player_controller:send(RouterPid, OpAtom, Payload).

get_pid(AccountId) ->
	world:build_pid(AccountId, "account").


handle_packet(AccountId, OpAtom, Callback, Payload) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {packet_rcvd, OpAtom, Callback, Payload}).



%% behavior callbacks

start_link(AccountId) ->
	Pid = get_pid(AccountId),
	gen_server:start_link(Pid, ?MODULE, {AccountId}, []).

init({AccountId}) ->
	io:format("account SERVER: started~n"),
	{ok, #state{account_id=AccountId}}.

handle_cast({packet_rcvd, OpAtom, Callback, Payload}, State = #state{account_id=AccountId}) ->
	Args = [{payload, Payload}, {account_id, AccountId}, {op_atom, OpAtom}],
	player_workers_sup:start_worker({Callback, Args}, AccountId),
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
