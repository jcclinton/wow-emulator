-module(player_model_controller).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([logged_out/2, logged_in/2]).

-include("include/binary.hrl").


-record(state, {
	account_id,
	guid
}).

%% public api
login(AccountId, Guid) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_fsm:send_event(Pid, {login, Guid}).


%% behavior callbacks

start_link(AccountId) ->
    gen_fsm:start_link(?MODULE, {AccountId}, []).

init({AccountId}) ->
	io:format("starting player_model_controller~n"),

	util:reg_proc(?MODULE, AccountId),
	{ok, logged_out, #state{account_id=AccountId}}.


logged_out({login, Guid}, State = #state{}) ->
	{next_state, logged_in, State#state{guid=Guid}};
logged_out(_, State) ->
	{next_state, logged_out, State}.


logged_in({logout, Guid}, State = #state{}) ->
	{next_state, logged_out, State#state{guid=0}};
logged_in(_, State) ->
	{next_state, logged_in, State}.



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
