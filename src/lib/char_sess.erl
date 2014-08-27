-module(char_sess).

-export([init/0, cleanup/0]).
-export([create/1, delete/1]).
-export([store_connected_client/2, get_session_key/1]).


-include("include/database_records.hrl").

% used to store ephemeral session data
-define(sess, character_session).

% used to store connected client session keys
-define(conn, connected_clients).


init() ->
	ets:new(?conn, [named_table, set, public]),
	ets:new(?sess, [named_table, set, public]).

cleanup() ->
	ets:delete(?conn),
	ets:delete(?sess).


% char session data

create(Guid) ->
	ets:insert(?sess, {Guid, #char_sess{}}).

delete(Guid) ->
	ets:delete(?sess, Guid).



% authorized connection data

store_connected_client(AccountId, Key) ->
	ets:insert(?conn, {AccountId, Key}).

get_session_key(AccountId) ->
	% for now, just crash if this client is authed
	[{_, Key}] = ets:lookup(?conn, AccountId),
	Key.

