-module(char_data).

-export([init/0, cleanup/0]).
-export([store_connected_client/2, get_session_key/1]).
-export([enum_chars/1, delete_char/1, create_char/1, get_char_data/1]).
-export([get_char_name/1, get_char_values/1, get_char_record/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").

-define(conn, connected_clients).
-define(char, characters).



init() ->
	ets:new(?conn, [named_table, set, public]),

	dets_store:open(?char, true),
	ok.

cleanup() ->
	ets:delete(?conn),

	dets_store:close(?char, true),
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
	dets_store:delete(?char, Guid, true).

get_char_record(Guid) ->
	{_Guid, _CharName, _AccountId, CharRecord, _Values} = get_char_data(Guid),
	CharRecord.

get_char_name(Guid) ->
	{_Guid, CharName, _AccountId, _CharRecord, _Values} = get_char_data(Guid),
	CharName.

get_char_values(Guid) ->
	{_Guid, _CharName, _AccountId, _CharRecord, Values} = get_char_data(Guid),
	Values.

create_char(CharData) ->
	dets_store:store_new(?char, CharData, true).

get_char_data(Guid) ->
	case dets_store:lookup(?char, Guid, true) of
		[] -> throw(badarg);
		[All] -> All
	end.
