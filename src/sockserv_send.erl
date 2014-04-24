-module(sockserv_send).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, locked/2, open/2]).

-include("include/binary.hrl").

-record(state, {socket,
				key_state
				}).

start_link(Socket, SKey) ->
    gen_fsm:start_link({Socket, SKey}, []).

init({Socket, SKey}) ->
	KState = {0, 0, SKey},
    {ok, send, #state{socket=Socket, key_state=KState}}.


send({send, <<ResponseOpCode?W, ResponseData/binary>>}, State = #state{socket=Socket, key_state=KState}) ->
	%% TODO store accept socket in ets
	Size = size(Data) + 4,
	Header = <<Size?WO, ResponseOpCode?W>>,
	{EncryptedHeader, NewKState} = world_crypto:encrypt(Header, KState),
	gen_tcp:send(State#state.socket, <<EncryptedHeader/binary, ResponseData/binary>>),
    {ok, send, #state{key_state=NewKState}}.
