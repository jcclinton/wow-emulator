-module(char_data).

-export([init/0, cleanup/0]).
-export([store_connected_client/2, get_session_key/1]).
-export([enum_chars/1, delete_char/1, create_char/1, get_char_data/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").

-define(conn, connected_clients).
-define(char, characters).



init() ->
	ets:new(?conn, [named_table, set, public]),
	ets:new(?char, [named_table, set, public]),
	ok.

cleanup() ->
	ets:delete(?conn),
	ets:delete(?char),
	ok.



% authorized connection data

store_connected_client(AccountId, Key) ->
	ets:insert(?conn, {AccountId, Key}).

get_session_key(AccountId) ->
	% for now, just crash if this client is authed
	[{_, Key}] = ets:lookup(?conn, AccountId),
	Key.




%char data

enum_chars(AccountId) ->
	ets:match_object(?char, {'_', '_', AccountId, '_', '_'}).

delete_char(Guid) ->
	ets:delete(?char, Guid).

create_char(CharData) ->
	% just decomposing this because it may change in the future
	{CharName, AccountId, Guid, CharRecord, Values} = CharData,
	ets:insert(?char, {Guid, CharName, AccountId, CharRecord, Values}).

get_char_data(Guid) ->
	case ets:lookup(?char, Guid) of
		[] -> throw(badarg);
		[All] -> All
	end.
