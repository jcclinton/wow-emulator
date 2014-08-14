-module(unit_spell).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([idle/2]).
-export([cast/3]).

-include("include/binary.hrl").


-record(state, {
	guid,
	spell,
	target_info
}).

%% public api
cast(CasterGuid, SpellId, TargetInfo) ->
	Pid = util:get_pid(?MODULE, CasterGuid),
	gen_fsm:send_event(Pid, {prepare, SpellId, TargetInfo}).


%% behavior callbacks

start_link(Guid) ->
    gen_fsm:start_link(?MODULE, {Guid}, []).

init({Guid}) ->
	io:format("starting unit spell~n"),

	util:reg_proc(?MODULE, Guid),

	{ok, idle, #state{guid=Guid}}.


idle({prepare, SpellId, TargetInfo}, State = #state{guid=Guid}) ->
	Spell = static_store:lookup_spell(SpellId),
	%io:format("spell: ~p~n", [Spell]),
	spell:prepare(Guid, TargetInfo, Spell),
	{next_state, idle, State#state{spell=Spell, target_info=TargetInfo}}.


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
