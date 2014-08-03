-module(char_sess).

-export([init/0, cleanup/0]).
-export([create/1, delete/1]).
-export([store_target/2, mark_update/2]).
-export([get_target/1]).


-include("include/database_records.hrl").

-define(sess, character_session).


init() ->
	ets:new(?sess, [named_table, set, public]).

cleanup() ->
	ets:delete(?sess).


create(Guid) ->
	ets:insert(?sess, {Guid, #char_sess{}}).

delete(Guid) ->
	ets:delete(?sess, Guid).

get_sess(Guid) ->
	[{Guid, Sess}] = ets:lookup(?sess, Guid),
	Sess.


get_target(Guid) ->
	Sess = get_sess(Guid),
	Sess#char_sess.target.



store_target(Guid, Target) ->
	Sess = get_sess(Guid),
	NewSess = Sess#char_sess{target=Target},
	ets:insert(?sess, {Guid, NewSess}).


mark_update(Guid, Indices) ->
	player_character:mark_update(Guid, Indices).
