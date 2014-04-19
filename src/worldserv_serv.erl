-module(worldserv_serv).
-behavior(gen_server).

-record(state, {
					socket,
					key
							 }).



-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("include/binary.hrl").
-include("include/world_records.hrl").


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


handle_info({tcp, _Socket, <<Length?WO, Opcode?L, Msg/binary>>}, State) ->
	ok = inet:setopts(State#state.socket, [{active, once}]),
	io:format("world SERVER: received client auth session~n"),
	{ResponseName, ResponseData, AccountId, Key1, Key2} = auth_session(Msg),
	ResponseOpCode = 494,
	Size = size(ResponseData) + 2,
	Header = <<Size:WO, ResponseOpCode?W>>,
	{EncryptedHeader, NewKey} = realm_crypto:encrypt(Header, Key1),
	gen_tcp:send(State#state.socket, <<EncryptedHeader/binary, ResponseData/binary>>,
	{noreply, S#state{socket=State#state.socket, key=NewKey}};
handle_info({tcp, _Socket, Msg}, State) ->
	io:format("world server: received unexpected tcp response: ~p~n", [Msg]),
	{noreply, State};
handle_info(Msg, State) ->
	io:format("world server: received unexpected message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(normal, _State) ->
	ok;
terminate(_Reason, _State) ->
	ok.


auth_session(Rest) ->
    {_, A, _}      = cmsg_auth_session(Rest),
    Data   = smsg_auth_response(),
    K      = world_crypto:encryption_key(A),
    EK     = #crypt_state{i=0, j=0, key=K},
    DK     = #crypt_state{i=0, j=0, key=K},
    {ok, Account} = find_by_name(A),
    {smsg_auth_response, Data, Account#account.id, EK, DK}.

cmsg_auth_session(<<Build?L, _Unk?L, Rest/binary>>) ->
    {Account, Key} = cmsg_auth_session_extract(Rest, ""),
    {Build, Account, Key};
cmsg_auth_session(_) ->
    {error, bad_cmsg_auth_session}.

cmsg_auth_session_extract(<<0?B, Rest/bytes>>, Account) ->
    {Account, binary_to_list(Rest)};
cmsg_auth_session_extract(<<Letter?B, Rest/binary>>, Account) ->
    cmsg_auth_session_extract(Rest, Account ++ [Letter]).

smsg_auth_response() ->
    <<12?B, 0?L, 0?B, 0?L, 1?B>>

find_by_name(Name) ->
    Result = do(qlc:q([X || X <- mnesia:table(account),
                            X#account.name =:= Name])),
    case Result of
    [Account] -> {ok, Account};
    _ -> {error, not_found}
    end.


%% private
buildAuthChallenge() ->
	Opcode = 492,
	Seed   = random:uniform(16#FFFFFFFF),
	Msg = [
	 <<Opcode?W>>,
	 <<Seed?L>>
	],
	Length = <<size(Msg):WO>>,
	[Length, Msg].
