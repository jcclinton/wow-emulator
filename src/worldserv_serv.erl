-module(worldserv_serv).
-behavior(gen_server).

-record(state, {
					socket
							 }).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("include/binary.hrl").


start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
	io:format("world SERVER: started~n"),
	gen_server:cast(self(), accept),
	{ok, #state{socket=Socket}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast(accept, S = #state{socket=ListenSocket}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	io:format("world SERVER: connected~n"),
	gen_server:cast(self(), connected),
	{noreply, S#state{socket=AcceptSocket}};
handle_cast(connected, State) ->
	Msg = buildAuthChallenge(),
	io:format("world SERVER: building auth message: ~p~n", [Msg]),
	gen_tcp:send(State#state.socket, Msg),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info({tcp, _Socket, Msg}, State) ->
	io:format("CLIENT: received unexpected tcp response: ~p~n", [Msg]),
	{noreply, State};
handle_info(_Msg, State) ->
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(normal, _State) ->
	ok;
terminate(_Reason, _State) ->
	ok.


%% private
buildAuthChallenge() ->
	Opcode = 492,
	Seed = 1,
	[
	 <<Opcode?W>>,
	 <<Seed?L>>
	].
