-module(player_controller).
-behavior(gen_server).

-record(state, {
	account_id,
	guid,
  parent_pid,
	send_pid
}).


-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([packet_received/3, login_char/2, logout_char/1, send/3, send/4]).


-include("include/binary.hrl").
-include("include/shared_defines.hrl").


%% public api

packet_received(AccountId, Opcode, Payload) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_server:cast(Pid, {packet_rcvd, Opcode, Payload}).

% normal send enqueues outgoing packets and sends them all at once
% packets can be sent straight through by passing in the fast atom as the type
send(AccountId, OpAtom, Payload) ->
	% enqueue is the default
	send(AccountId, OpAtom, Payload, enqueue).
send(AccountId, OpAtom, Payload, Type) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_server:cast(Pid, {send_to_client, OpAtom, Payload, Type}).


login_char(AccountId, Guid) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_server:cast(Pid, {login_char, Guid}).

logout_char(AccountId) ->
	Pid = util:get_pid(?MODULE, AccountId),
	gen_server:cast(Pid, logout_char).




%% behavior callbacks

start_link(AccountId, SendPid, ParentPid) ->
	gen_server:start_link(?MODULE, {AccountId, SendPid, ParentPid}, []).

init({AccountId, SendPid, ParentPid}) ->
	io:format("controller SERVER: started~n"),
	util:reg_proc(?MODULE, AccountId),
	{ok, #state{account_id=AccountId, send_pid=SendPid, parent_pid=ParentPid, guid=0}}.


handle_cast({packet_rcvd, Opcode, Payload}, State = #state{account_id=AccountId, guid=Guid}) ->
	%io:format("looking up opcode ~p for ~p~n", [Opcode, AccountId]),
	OpAtom = opcodes:get_atom_by_num(Opcode),

	case opcodes:get_callback_by_num(OpAtom) of
		none ->
			io:format("unknown callback: ~p~n", [OpAtom]),
			ok;
		Callback ->

			% the args are roughly in order by how often they are accessed
			Args1 = [{payload, Payload}, {op_atom, OpAtom}, {account_id, AccountId}],
			Args = if Guid > 0 -> [{guid, Guid} | Args1];
				Guid == 0 -> Args1
			end,

			% if a character callback type is called and the guid is 0
			% then do nothing
			% you cant call a character callback when a player is not logged in
			if Callback#callback.type /= character orelse Guid > 0 ->
					player_workers_sup:start_worker({Callback, Args}, AccountId);
				true ->
					io:format("character callback type with guid = 0 called~n"),
					ok
			end
	end,
	{noreply, State};
handle_cast({send_to_client, OpAtom, Payload, Type}, State=#state{send_pid = SendPid}) ->
	Opcode = opcodes:get_num_by_atom(OpAtom),
	%io:format("sending ~p to client~n", [OpAtom]),
	player_send:send_msg(SendPid, Opcode, Payload, Type),
	{noreply, State};
handle_cast({login_char, Guid}, State=#state{parent_pid = ParentPid, guid=OldGuid}) ->
	if OldGuid > 0 -> throw(badarg);
		OldGuid == 0 -> ok
	end,

	char_sess:create(Guid),

	Name = unit_ephemeral_sup,
	Spec = {Name,
		{Name, start_link, [Guid, player]},
		permanent, 2000, supervisor, [Name]},
	{ok, _Pid} = supervisor:start_child(ParentPid, Spec),

	{noreply, State#state{guid=Guid}};
handle_cast(logout_char, State=#state{parent_pid = ParentPid, guid=Guid}) ->
	char_sess:delete(Guid),

	Name = unit_ephemeral_sup,
	supervisor:terminate_child(ParentPid, Name),
	supervisor:delete_child(ParentPid, Name),
	{noreply, State#state{guid=0}};
handle_cast(Msg, State) ->
	io:format("unknown casted message: ~p~n", [Msg]),
	{noreply, State}.


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	io:format("WORLD: shutting down controller~n"),
	ok.
