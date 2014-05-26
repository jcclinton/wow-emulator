-module(world).
-behavior(gen_server).

-record(state, {
								current_guid
							 }).


-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_guid/0]).


-include("include/binary.hrl").
-include("include/database_records.hrl").

%%% public api
get_guid() ->
	gen_server:call(?MODULE, new_guid).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("world: started~n"),
	{ok, #state{current_guid=1}}.


handle_call(new_guid, _From, State = #state{current_guid=Guid}) ->
	{reply, Guid, State#state{current_guid=Guid+1}};
handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.



handle_info(upgrade, State) ->
	%% loads latest code
	?MODULE:handle_info(do_upgrade, State),
	{noreply, State};
handle_info(_Msg, State) ->
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
