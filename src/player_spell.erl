-module(player_spell).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([idle/2]).
-export([cast/3]).

-include("include/binary.hrl").


-record(state, {
	account_id,
	guid
}).

%% public api
cast(CasterGuid, SpellId, TargetInfo) ->
	Pid = util:get_pid(?MODULE, CasterGuid),
	gen_fsm:send_event(Pid, {prepare, CasterGuid, SpellId, TargetInfo}).


%% behavior callbacks

start_link(AccountId, Guid) ->
    gen_fsm:start_link(?MODULE, {AccountId, Guid}, []).

init({AccountId, Guid}) ->
	io:format("starting player spell~n"),

	util:reg_proc(?MODULE, Guid),

	{ok, idle, #state{account_id=AccountId, guid=Guid}}.


idle(_, State) ->
	{next_state, idle, State}.


handle_info(_Info, State, Data) ->
	{next_state, State, Data}.

handle_event(_Event, State, Data) ->
	{next_state, State, Data}.

handle_sync_event(_Event, _From, State, Data) ->
	{next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
	ok.

code_change(_OldVsn, State, Data, _Extra) ->
	{ok, State, Data}.
