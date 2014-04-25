-module(sockserv_controller).
-behavior(gen_server).

-record(state, {
  user,
  sess_key,
  parent_pid,
  send_pid
							 }).
-record(user, {
  pos
							 }).


-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-compile([export_all]).


-include("include/binary.hrl").


start_link(ParentPid, SKey) ->
	gen_server:start_link(?MODULE, {ParentPid, SKey}, []).

init({ParentPid, SKey}) ->
	io:format("controller SERVER: started~n"),
	gen_server:cast(self(), init),
	{ok, #state{user=#user{}, parent_pid=ParentPid, sess_key=SKey}}.


%% receiver has accepted connection
%% start send process
handle_call({tcp_accept_socket, Socket}, _From, S = #state{sess_key=SKey, parent_pid=ParentPid}) ->
	SendSupPid = get_sibling_pid(ParentPid, sockserv_send_sup),
	{ok, SendPid} = supervisor:start_child(SendSupPid, [Socket, SKey]),
	{reply, ok, S#state{send_pid=SendPid}};
handle_call(_E, _From, State) ->
	{reply, ok, State}.

%% initialize controller
%% start rcv process
handle_cast(init, State = #state{parent_pid=ParentPid, sess_key=SKey}) ->
	RcvSupPid = get_sibling_pid(ParentPid, sockserv_rcv_sup),
	supervisor:start_child(RcvSupPid, [SKey, self()]),
	{noreply, State};
handle_cast({tcp_accept_challenge, Msg} State) ->
	{_ResponseName, ResponseData, _AccountId, KTup} = auth_session(Msg),
	ResponseOpCode = 494,
	Size = size(ResponseData) + 2,
	Header = <<Size?WO, ResponseOpCode?W>>,

	routeData(self(), Msg),
	{noreply, State};
handle_cast({tcp_packet_rcvd, <<Opcode?WO, Payload/binary>>}, S = #state{user=User}) ->
	io:format("received opcode ~p with payload ~p~n", [Opcode, Payload]),
	{_M, _F} = opcode_patterns:lookup_function(Opcode),
	_A = [User, Payload],
	%{NewUser, {Pids, Msg}} = apply({M,F,A}),
	NewUser = User,
	%routeData(Pids, Msg),
	{noreply, S#state{user=NewUser}};
handle_cast({send_to_client, Msg}, S=#state{send_pid = SendPid}) ->
	gen_fsm:send_event(SendPid, {send, Msg}),
	{noreply, S};
handle_cast(Msg, S) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, S}.

handle_info(upgrade, State) ->
	%% loads latest code
	?MODULE:handle_info(do_upgrade, State),
	{noreply, State};
handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(normal, _State) ->
	ok;
terminate(_Reason, _State) ->
	ok.


%% private

get_sibling_pid(ParentPid, SiblingId) ->
	ChildList = supervisor:which_children(ParentPid),
	[{_, Pid, _, _}] = lists:filter(fun({Id, _Child, _Type, _Modules}) ->
		Id == SiblingId
	end, ChildList),
	Pid.



%% takes a list of pids and a formatted message
%% routes the message to the pids
routeData([], _) -> ok;
routeData([Pid|Rest], Msg) ->
	routeData(Pid, Msg),
	routeData(Rest, Msg);
routeData(Pid, Msg) when erlang:is_pid(Pid) ->
	gen_server:cast(Pid, {send_to_client, Msg}).


auth_session(Rest) ->
    {_, A, _}      = cmsg_auth_session(Rest),
    Data   = smsg_auth_response(),
    K      = world_crypto:encryption_key(A),
    KTup     = {0, 0, K},
		AccountId = logon_lib:getUsername(),
    {smsg_auth_response, Data, AccountId, KTup}.

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
    <<12?B, 0?L, 0?B, 0?L, 1?B>>.
