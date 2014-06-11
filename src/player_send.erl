-module(player_send).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([send/2]).

-include("include/binary.hrl").

-record(state, {socket,
	key_state
				}).

start_link(Socket, KeyState) ->
    gen_fsm:start_link(?MODULE, {Socket, KeyState}, []).

init({Socket, KeyState}) ->
	io:format("WORLD: starting send~n"),
	process_flag(trap_exit, true),
    {ok, send, #state{socket=Socket, key_state=KeyState}}.


send({send, <<ResponseOpcode?W, ResponseData/binary>>}, State = #state{socket=Socket, key_state=KeyState}) ->
	%% TODO store socket in ets
	Length = size(ResponseData) + 2,
	Header = <<Length?WO, ResponseOpcode?W>>,
	io:format("sending opcode ~p with length ~p~n", [ResponseOpcode, Length]),
	{EncryptedHeader, NewKeyState} = world_crypto:encrypt(Header, KeyState),
	Packet = <<EncryptedHeader/binary, ResponseData/binary>>,
	%io:format("sending packet: ~p~n", [Packet]),
	gen_tcp:send(Socket, Packet),
	{next_state, send, State#state{key_state=NewKeyState}}.

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
