-module(player_send).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([send/2]).
-export([upgrade/0]).

-include("include/binary.hrl").
-include("include/network_defines.hrl").

-record(state, {socket,
	key_state,
	hdr_len
				}).

start_link(Socket, KeyState) ->
    gen_fsm:start_link(?MODULE, {Socket, KeyState}, []).

init({Socket, KeyState}) ->
	io:format("WORLD: starting send~n"),
	process_flag(trap_exit, true),
    {ok, send, #state{socket=Socket, key_state=KeyState, hdr_len=?SEND_HDR_LEN}}.


send({send, {OpAtom, Payload}}, State = #state{socket=Socket, key_state=KeyState, hdr_len=HdrLen}) ->
	NewKeyState = world_crypto:send_packet(OpAtom, Payload, HdrLen, KeyState, Socket, true),
	{next_state, send, State#state{key_state=NewKeyState}}.

upgrade() -> ok.

%% callbacks
handle_info(_Info, State, Data) ->
	{next_state, State, Data}.

handle_event(_Event, State, Data) ->
	{next_state, State, Data}.

handle_sync_event(_Event, _From, State, Data) ->
	{next_state, State, Data}.

terminate(_Reason, State, _Data) ->
	io:format("WORLD SEND: closing connected realm_socket~n"),
	catch gen_tcp:close(State#state.socket),
	ok.

code_change(_OldVsn, State, Data, _Extra) ->
	{ok, State, Data}.
