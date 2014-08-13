-module(player_send).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([send/2]).
-export([upgrade/0]).
-export([send_msg/4]).

-include("include/binary.hrl").
-include("include/network_defines.hrl").
-include("include/shared_defines.hrl").


-record(state, {
	socket,
	key_state,
	hdr_len,
	queue,
	timer
}).

%% public api
send_msg(SendPid, Opcode, Payload, Type) ->
	Msg = case Type of
		% to add a new priority:
		% add it here
		% add it as a shared_define
		?send_priority_fast -> {fast_send, Opcode, Payload};
		?send_priority_enqueue -> {enqueue, Opcode, Payload}
	end,
	gen_fsm:send_event(SendPid, Msg).



%% behavior callbacks

start_link(Socket, KeyState) ->
    gen_fsm:start_link(?MODULE, {Socket, KeyState}, []).

init({Socket, KeyState}) ->
	io:format("WORLD: starting send~n"),
	process_flag(trap_exit, true),
    {ok, send, #state{socket=Socket, key_state=KeyState, hdr_len=?SEND_HDR_LEN, queue=queue:new(), timer=none}}.


send({enqueue, Opcode, Payload}, State = #state{queue=Queue, timer=Timer}) ->
	% enqueue this data
	% start timer if it has not already been started
	%io:format("enqueue ~p~n", [Opcode]),
	NewQueue = queue:in({Opcode, Payload}, Queue),
	NewTimer = if Timer /= none ->
			Timer;
		Timer == none ->
			{ok, Tref} = timer:apply_after(?game_tick, gen_fsm, send_event, [self(), flush]),
			Tref
	end,
	{next_state, send, State#state{queue=NewQueue, timer=NewTimer}};
send(flush, State = #state{socket=Socket, key_state=KeyState, hdr_len=HdrLen, queue=Queue}) ->
	%io:format("send~n"),
	% send all data from queue to client
	try network:send_queue(Queue, HdrLen, KeyState, Socket, _ShouldEncrypt=true) of
		NewKeyState -> {next_state, send, State#state{key_state=NewKeyState, queue=queue:new(), timer=none}}
	catch
		Error -> {stop, Error, State}
	end;
send({fast_send, Opcode, Payload}, State = #state{socket=Socket, key_state=KeyState, hdr_len=HdrLen, queue=Queue, timer=Timer}) ->
	%io:format("fast send ~p~n", [Opcode]),
	if Timer /= none -> timer:cancel(Timer);
		Timer == none -> ok
	end,
	% add packet to front of queue
	% then flush entire queue
	NewQueue = queue:in_r({Opcode, Payload}, Queue),
	try network:send_queue(NewQueue, HdrLen, KeyState, Socket, _ShouldEncrypt=true) of
		NewKeyState -> {next_state, send, State#state{key_state=NewKeyState, queue=queue:new(), timer=none}}
	catch
		Error -> {stop, Error, State}
	end.

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
