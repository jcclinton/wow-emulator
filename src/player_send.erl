-module(player_send).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([accumulate/2, idle/2]).
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
		fast -> {fast_send, Opcode, Payload};
		enqueue -> {enqueue, Opcode, Payload}
	end,
	gen_fsm:send_event(SendPid, Msg).

upgrade() -> ok.



%% behavior callbacks

start_link(Socket, KeyState) ->
    gen_fsm:start_link(?MODULE, {Socket, KeyState}, []).

init({Socket, KeyState}) ->
	io:format("WORLD: starting send~n"),
	process_flag(trap_exit, true),
    {ok, idle, #state{socket=Socket, key_state=KeyState, hdr_len=?SEND_HDR_LEN, queue=none, timer=none}}.


idle({enqueue, Opcode, Payload}, State = #state{}) ->
	% create new queue with this data
	Queue = queue:in({Opcode, Payload}, queue:new()),
	% start timer
	{ok, Timer} = timer:apply_after(?game_tick, gen_fsm, send_event, [self(), flush]),
	{next_state, accumulate, State#state{queue=Queue, timer=Timer}};
idle({fast_send, Opcode, Payload}, State = #state{socket=Socket, key_state=KeyState, hdr_len=HdrLen}) ->
	try network:send_packet(Opcode, Payload, HdrLen, KeyState, Socket, _ShouldEncrypt=true) of
		NewKeyState -> {next_state, idle, State#state{key_state=NewKeyState}}
	catch
		Error -> {stop, Error, State}
	end.


accumulate({enqueue, Opcode, Payload}, State = #state{queue=Queue}) ->
	% enqueue this data
	NewQueue = queue:in({Opcode, Payload}, Queue),
	{next_state, accumulate, State#state{queue=NewQueue}};
accumulate(flush, State = #state{socket=Socket, key_state=KeyState, hdr_len=HdrLen, queue=Queue}) ->
	% send all data from queue to client
	try network:send_queue(Queue, HdrLen, KeyState, Socket, _ShouldEncrypt=true) of
		NewKeyState -> {next_state, idle, State#state{key_state=NewKeyState, queue=none, timer=none}}
	catch
		Error -> {stop, Error, State}
	end;
accumulate({fast_send, Opcode, Payload}, State = #state{queue=Queue, timer=Timer}) ->
	timer:cancel(Timer),
	% insert in front of queue
	% not a big deal, can be done either way
	NewQueue = queue:in_r({Opcode, Payload}, Queue),
	accumulate(flush, State#state{queue=NewQueue}).



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
