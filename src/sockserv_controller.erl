-module(sockservcontroller).
-behavior(gen_server).

-record(state, {
  user,
  sess_key,
  parent_pid
							 }).
-record(user, {
  pos
							 }).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-compile([export_all]).


-include("include/binary.hrl").


start_link(ParentPid, Skey) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {ParentPid}, []).

init({ParentPid, SKey}) ->
	gen_server:cast(self(), init),
	{ok, #state{user=#user{}, parent_pid=ParentPid, sess_key=SKey}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

%% initialize controller
%% start rcv process
handle_cast(init, S = #state{parent_pid=ParentPid, sess_key=SKey}) ->
	RcvSupPid = get_sibling_pid(ParentPid, sockserv_rcv_sup),
	supervisor:start_child(RcvSupPid, [SKey, self(), ParentPid]).
	{noreply, S};
%% receiver has accepted connection
%% start send process
handle_cast({tcp_accept_socket, Socket}, S = #state{user=User, sess_key=SKey, parent_pid=ParentPid}) ->
	SendSupPid = get_sibling_pid(ParentPid, sockserv_send_sup),
	supervisor:start_child(SendSupPid, [Socket, SKey]),
	{noreply, S};
handle_cast({tcp_packet_rcvd, {Opcode?WO, Payload/binary}}, S = #state{user=User}) ->
	{M, F} = opcode_patterns:lookup_function(Opcode),
	A = [User],
	NewUser = apply({M,F,A}),
	{noreply, S#state{user=NewUser}};
handle_cast(Msg, S) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, S};

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
	[{_, Pid, _, _}] = lists:filter(fun([Id, _Child, _Type, _Modules]) ->
		Id == SiblingId
	end, ChildList),
	Pid.
