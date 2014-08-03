-module(player_account).
-behavior(gen_server).

-record(state, {
	account_id
}).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([handle_packet/4]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").


%% public api

handle_packet(AccountId, OpAtom, Callback, Payload) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {packet_rcvd, OpAtom, Callback, Payload}).



%% behavior callbacks

start_link(AccountId) ->
	gen_server:start_link(?MODULE, {AccountId}, []).

init({AccountId}) ->
	io:format("account SERVER: started~n"),
	Key = build_pid_key(AccountId),
	gproc:reg({n, l, Key}, none),
	{ok, #state{account_id=AccountId}}.

handle_cast({packet_rcvd, OpAtom, Callback, Payload}, State = #state{account_id=AccountId}) ->
	Args = [{account_id, AccountId}, {payload, Payload}, {op_atom, OpAtom}],
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




%%%%%%%%%%%%%%%%
%% private


build_pid_key(AccountId) ->
	AccountId ++ "account".

get_pid(AccountId) ->
	Key = build_pid_key(AccountId),
	gproc:lookup_pid({n, l, Key}).

