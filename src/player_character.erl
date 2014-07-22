-module(player_character).
-behavior(gen_server).

-record(state, {
	account_id,
	name,
	values,
	char,
	guid
}).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_pid/1, send/3, handle_packet/5]).


-include("include/binary.hrl").


%% public api

send(Name, OpAtom, Payload) ->
	RouterPid = player_controller:get_pid(Name),
	player_controller:send(RouterPid, OpAtom, Payload).

get_pid(AccountId) ->
	world:build_pid(AccountId, "char").


handle_packet(AccountId, OpAtom, M, F, Payload) ->
	Pid = get_pid(AccountId),
	gen_server:cast(Pid, {packet_rcvd, OpAtom, M, F, Payload}).



%% behavior callbacks

start_link(AccountId, Guid) ->
	Pid = get_pid(AccountId),
	gen_server:start_link(Pid, ?MODULE, {AccountId, Guid}, []).

init({AccountId, Guid}) ->
	io:format("char SERVER: started~n"),
	{Guid, CharName, AccountId, CharRecord, Values} = char_data:get_char_data(Guid),
	{ok, #state{account_id=AccountId, name=CharName, char=CharRecord, values=Values, guid=Guid}}.

handle_cast({packet_rcvd, OpAtom, M, F, Payload}, State = #state{account_id=AccountId, name=CharName, char=CharRecord, values=Values, guid=Guid}) ->
	Args = [{payload, Payload}, {account_id, AccountId}, {op_atom, OpAtom}, {guid, Guid}, {char_name, CharName}, {char, CharRecord}, {values, Values}],
	util:call(M, F, Args, AccountId),
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
	io:format("WORLD: shutting down char~n"),
	ok.
