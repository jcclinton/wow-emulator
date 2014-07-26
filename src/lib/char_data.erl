-module(char_data).

-export([init/0, cleanup/0]).
-export([store_connected_client/2, get_session_key/1]).
-export([enum_chars/1, delete_char/1, create_char/5]).
-export([get_values/1, get_char/1, get_char_values_pair/1, get_account_id/1, get_char_spells/1]).
-export([update_char/2, update_coords/5, update_values/2, add_spell/2]).
-export([init_session/1, close_session/1]).
-export([store_selection/2, store_mask/2, clear_mask/1]).
-export([get_mask/1]).

-include("include/binary.hrl").
-include("include/database_records.hrl").

-define(conn, connected_clients).
-define(char_sess, characters_session).

-define(char_val, characters_values).
-define(char_rec, characters_record).
-define(char_acc, characters_account).
-define(char_spells, characters_spells).



init() ->
	ets:new(?conn, [named_table, set, public]),
	ets:new(?char_sess, [named_table, set, public]),

	dets_store:open(?char_val, true),
	dets_store:open(?char_rec, true),
	dets_store:open(?char_acc, true),
	dets_store:open(?char_spells, true),
	ok.

cleanup() ->
	ets:delete(?conn),
	ets:delete(?char_sess),

	dets_store:close(?char_val, true),
	dets_store:close(?char_rec, true),
	dets_store:close(?char_acc, true),
	dets_store:close(?char_spells, true),
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
	EmptyMask = update_mask:empty(),
	ets:insert_new(?char_sess, {Guid, #char_sess{update_mask=EmptyMask}}).

close_session(Guid) ->
	ets:delete(?char_sess, Guid).

store_selection(Guid, Target) ->
	Sess = get_sess(Guid),
	NewSess = Sess#char_sess{target=Target},
	ets:insert(?char_sess, {Guid, NewSess}).

store_mask(Guid, Mask) ->
	Sess = get_sess(Guid),
	NewSess = Sess#char_sess{update_mask=Mask},
	ets:insert(?char_sess, {Guid, NewSess}).

get_mask(Guid) ->
	Sess = get_sess(Guid),
	Sess#char_sess.update_mask.

clear_mask(Guid) ->
	EmptyMask = update_mask:empty(),
	store_mask(Guid, EmptyMask).

get_sess(Guid) ->
	[{Guid, Sess}] = ets:lookup(?char_sess, Guid),
	Sess.



% persistent char data

enum_chars(AccountId) ->
	Chars = ets:match_object(?char_acc, {'_', AccountId}),
	lists:map(fun({Guid, _}) ->
		get_char_values_pair(Guid)
	end, Chars).


get_char(Guid) ->
	get_char_data(Guid, ?char_rec).

get_char_spells(Guid) ->
	get_char_data(Guid, ?char_spells).

get_account_id(Guid) ->
	get_char_data(Guid, ?char_acc).

get_values(Guid) ->
	get_char_data(Guid, ?char_val).

get_char_values_pair(Guid) ->
	Char = get_char(Guid),
	Values = get_values(Guid),
	{Char, Values}.

get_char_data(Guid, Tab) ->
	case dets_store:lookup(Tab, Guid, true) of
		[] -> throw(badarg);
		[{Guid, Val}] -> Val
	end.



delete_char(Guid) ->
	dets_store:delete(?char_val, Guid, true),
	dets_store:delete(?char_acc, Guid, true),
	dets_store:delete(?char_rec, Guid, true),
	dets_store:delete(?char_spells, Guid, true).

create_char(Guid, AccountId, Char, Values, Spells) when is_integer(Guid), is_binary(Values), is_record(Char, char), is_list(AccountId), is_record(Spells, char_spells) ->
	dets_store:store_new(?char_val, {Guid, Values}, true),
	dets_store:store_new(?char_rec, {Guid, Char}, true),
	dets_store:store_new(?char_acc, {Guid, AccountId}, true),
	dets_store:store_new(?char_spells, {Guid, Spells}, true).

update_values(Guid, Values) when is_binary(Values) ->
	dets_store:store(?char_val, {Guid, Values}, true).

update_char(Guid, Char) when is_record(Char, char) ->
	dets_store:store(?char_rec, {Guid, Char}, true).

update_coords(Guid, X, Y, Z, O) ->
	Char = get_char(Guid),
	NewChar = Char#char{x=X, y=Y, z=Z, orient=O},
	update_char(Guid, NewChar).

add_spell(Guid, SpellId) ->
	Record = get_char_spells(Guid),
	Ids = Record#char_spells.ids,
	InList = lists:any(fun(Id) -> SpellId == Id end, Ids),
	if not InList ->
			NewList = [SpellId|Ids],
			NewRecord = Record#char_spells{ids=NewList},
			dets_store:store(?char_spells, {Guid, NewRecord}, true);
		InList -> ok
	end.

