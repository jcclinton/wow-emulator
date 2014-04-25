-module(sockserv_send).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([send/2]).

-include("include/binary.hrl").

-record(state, {socket,
				key_state
				}).

start_link(Socket, KState) ->
    gen_fsm:start_link(?MODULE, {Socket, KState}, []).

init({Socket, KState}) ->
	io:format("starting send~n"),
    {ok, send, #state{socket=Socket, key_state=KState}}.


send({send, <<ResponseOpCode?W, ResponseData/binary>>}, State = #state{socket=Socket, key_state=KState}) ->
	%% TODO store socket in ets
	Size = size(ResponseData) + 4,
	Header = <<Size?WO, ResponseOpCode?W>>,
	{EncryptedHeader, NewKState} = world_crypto:encrypt(Header, KState),
	gen_tcp:send(Socket, <<EncryptedHeader/binary, ResponseData/binary>>),
    {next_state, send, State#state{key_state=NewKState}}.

%% callbacks
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
