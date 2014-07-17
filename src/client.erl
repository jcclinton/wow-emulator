-module(client).
-behavior(gen_server).

-record(state, {
	socket,
	account="ALICE"
}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
%% api
-export([close/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


close(Pid) ->
	gen_server:cast(Pid, close).
	


%% public
start_link() ->
	gen_server:start_link(?MODULE, {}, []).



init({}) ->
	io:format("player CLIENT: started~n"),
	gen_server:cast(self(), connect),
	{ok, #state{}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast(connect, State) ->
	process_flag(trap_exit, true),
	{ok, Port} = application:get_env(world_port),
	{ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, true}]),
	{noreply, State#state{socket=Socket}};
handle_cast(close, State) ->
	catch gen_tcp:close(State#state.socket),
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, _Socket, <<_?WO, 16#1EC?W, _/binary>>}, State = #state{account=Account, socket=Socket}) ->
	io:format("CLIENT: received auth challenge~n"),
	Opcode = opcode_patterns:getNumByAtom(cmsg_challenge_accept),
	Name = list_to_binary(Account),
	Payload = <<Name/binary, 0?B, 0?B>>,
	Length = byte_size(Payload) + 4,
	Packet = <<Length?WO, Opcode?L, Payload/binary>>,
	%gen_tcp:send(Socket, Packet),
	{noreply, State};
handle_info(Msg, State) ->
	io:format("CLIENT: received unexpected response: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
