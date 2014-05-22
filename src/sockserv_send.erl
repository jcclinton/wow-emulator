-module(sockserv_send).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, handle_sync_event/4, handle_event/3,
				 handle_info/3, terminate/3, code_change/4]).
-export([send/2]).

-include("include/binary.hrl").

-record(state, {socket,
	pair_pid
				}).

start_link(Socket, PairPid) ->
    gen_fsm:start_link(?MODULE, {Socket, PairPid}, []).

init({Socket, PairPid}) ->
	io:format("starting send~n"),
    {ok, send, #state{socket=Socket, pair_pid=PairPid}}.


send({send, <<ResponseOpCode?W, ResponseData/binary>>}, State = #state{socket=Socket, pair_pid=PairPid}) ->
	%% TODO store socket in ets
	Size = size(ResponseData) + 4,
	Header = <<Size?WO, ResponseOpCode?W>>,
	KState = gen_server:call(PairPid, key_state),
	{EncryptedHeader, NewKeyState} = world_crypto:encrypt(Header, KState),
	gen_server:cast(PairPid, {new_key_state, NewKeyState}),
	Packet = <<EncryptedHeader/binary, ResponseData/binary>>,
	io:format("sending world data: ~p~nand with header: ~p~n", [Packet, Header]),
	gen_tcp:send(Socket, Packet),
    {next_state, send, State}.

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
