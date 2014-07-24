-module(char_data).

-export([init/0, cleanup/0]).
-export([store_connected_client/2, get_session_key/1]).
-export([enum_chars/1, delete_char/1, create_char/1, get_char_data/1]).
-export([get_char_values/1, get_char_record/1, get_char_record_value/1, get_account_id/1]).
-export([update_char/1, update_coords/5]).
-export([init_session/1, close_session/1]).
-export([store_selection/2, store_mask/2, clear_mask/1]).
-export([get_mask/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").

-define(conn, connected_clients).
-define(char_sess, characters_session).
-define(char, characters).



init() ->
	ets:new(?conn, [named_table, set, public]),
	ets:new(?char_sess, [named_table, set, public]),

	dets_store:open(?char, true),
	ok.

cleanup() ->
	ets:delete(?conn),
	ets:delete(?char_sess),

	dets_store:close(?char, true),
	ok.



% authorized connection data

store_connected_client(AccountId, Key) ->
	ets:insert(?conn, {AccountId, Key}).

get_session_key(AccountId) ->
	% for now, just crash if this client is authed
	[{_, Key}] = ets:lookup(?conn, AccountId),
	Key.


% session data
init_session(Guid) ->
	ets:insert_new(?char_sess, {Guid, #char_sess{}}).

close_session(Guid) ->
	ets:delete(?char_sess, Guid).

store_selection(Guid, Target) ->
	[{Guid, Sess}] = ets:lookup(?char_sess, Guid),
	NewSess = Sess#char_sess{target=Target},
	ets:insert(?char_sess, {Guid, NewSess}).

store_mask(Guid, Mask) ->
	[{Guid, Sess}] = ets:lookup(?char_sess, Guid),
	NewSess = Sess#char_sess{update_mask=Mask},
	ets:insert(?char_sess, {Guid, NewSess}).

get_mask(Guid) ->
	[{Guid, Sess}] = ets:lookup(?char_sess, Guid),
	Sess#char_sess.update_mask.

clear_mask(Guid) ->
	TotalCount = update_fields:get_total_count(player),
	EmptyMask = update_mask:empty(TotalCount - 1),
	store_mask(Guid, EmptyMask).



% persistent char data

enum_chars(AccountId) ->
	ets:match_object(?char, {'_', AccountId, '_', '_'}).

delete_char(Guid) ->
	dets_store:delete(?char, Guid, true).

get_char_record(Guid) ->
	{_Guid, _AccountId, CharRecord, _Values} = get_char_data(Guid),
	CharRecord.

get_account_id(Guid) ->
	{_Guid, AccountId, _CharRecord, _Values} = get_char_data(Guid),
	AccountId.

get_char_values(Guid) ->
	{_Guid, _AccountId, _CharRecord, Values} = get_char_data(Guid),
	Values.

get_char_record_value(Guid) ->
	{_Guid, _AccountId, CharRecord, Values} = get_char_data(Guid),
	{CharRecord, Values}.

get_char_data(Guid) ->
	case dets_store:lookup(?char, Guid, true) of
		[] -> throw(badarg);
		[All] -> All
	end.


create_char(CharData) ->
	dets_store:store_new(?char, CharData, true).

update_char(CharData) ->
	dets_store:store(?char, CharData, true).

update_coords(Guid, X, Y, Z, O) ->
	{Guid, AccountId, Char, Values} = char_data:get_char_data(Guid),
	NewChar = Char#char{x=X, y=Y, z=Z, orient=O},
	CharData = {Guid, AccountId, NewChar, Values},
	char_data:update_char(CharData).
