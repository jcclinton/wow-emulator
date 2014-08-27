-module(char_data).

-export([init/0, cleanup/0]).
-export([store_connected_client/2, get_session_key/1]).
-export([enum_char_guids/1, delete_char/1, create_char/8]).
-export([equip_starting_items/1]).
-export([get_values/1, get_stored_values/1, get_char_misc/1, get_char_name/1, get_char_move/1, get_account_id/1, get_char_spells/1, get_action_buttons/1, get_slot_values/1]).
-export([update_char_misc/2, update_char_move/2, update_coords/6, add_spell/2, create_action_buttons/1, update_action_button/2, update_slot_values/2]).
-export([take_damage/2]).

-include("include/binary.hrl").
-include("include/database_records.hrl").
-include("include/shared_defines.hrl").
-include("include/character.hrl").

-define(conn, connected_clients).

-define(char_val, characters_values).
-define(char_name, characters_names).
-define(char_misc, characters_miscellaneous).
-define(char_acc, characters_account).
-define(char_spells, characters_spells).
-define(char_btns, characters_btns).
-define(char_mv, characters_movement).
-define(char_items, characters_items).


get_char_tabs() ->
	[
		?char_name,
		?char_misc,
		?char_mv,
		?char_acc,
		?char_spells,
		?char_btns,
		?char_items
	].



init() ->
	ets:new(?conn, [named_table, set, public]),

	lists:foreach(fun(Tab) ->
		dets_store:open(Tab, true)
	end, get_char_tabs()),

	dets_store:open(?char_val, false),
	ok.

cleanup() ->
	ets:delete(?conn),

	lists:foreach(fun(Tab) ->
		dets_store:close(Tab, true)
	end, get_char_tabs()),
	dets_store:close(?char_val, false),
	ok.




% authorized connection data

store_connected_client(AccountId, Key) ->
	ets:insert(?conn, {AccountId, Key}).

get_session_key(AccountId) ->
	% for now, just crash if this client is authed
	[{_, Key}] = ets:lookup(?conn, AccountId),
	Key.




% persistent char data

enum_char_guids(AccountId) ->
	CharsGuids = ets:match_object(?char_acc, {'_', AccountId}),
	lists:map(fun({Guid, _}) -> Guid end, CharsGuids).


get_char_misc(Guid) ->
	get_char_data(Guid, ?char_misc).

get_char_name(Guid) ->
	get_char_data(Guid, ?char_name).

get_char_move(Guid) ->
	get_char_data(Guid, ?char_mv).

get_char_spells(Guid) ->
	get_char_data(Guid, ?char_spells).

get_action_buttons(Guid) ->
	get_char_data(Guid, ?char_btns).

get_account_id(Guid) ->
	get_char_data(Guid, ?char_acc).

get_slot_values(Guid) ->
	get_char_data(Guid, ?char_items).

get_stored_values(Guid) ->
	get_char_data(Guid, ?char_val, false).

get_values(Guid) ->
	%get_char_data(Guid, ?char_val).
	player_state:get_values(Guid).

get_char_data(Guid, Tab) ->
	get_char_data(Guid, Tab, true).
get_char_data(Guid, Tab, Stored) ->
	case dets_store:lookup(Tab, Guid, Stored) of
		[] -> throw(badarg);
		[{Guid, Val}] -> Val
	end.





delete_char(Guid) ->
	lists:foreach(fun(Tab) ->
		dets_store:delete(Tab, Guid, true)
	end, get_char_tabs()),
	ok.


create_char(Guid, AccountId, CharName, CharMisc, CharMv, Values, Spells, ActionButtons) when is_integer(Guid), is_binary(Values), is_binary(CharName), is_record(CharMisc, char_misc), is_record(CharMv, char_move), is_binary(AccountId), is_record(Spells, char_spells), is_binary(ActionButtons) ->
	DetsValues = [
		{?char_name, CharName},
		{?char_misc, CharMisc},
		{?char_mv, CharMv},
		{?char_acc, AccountId},
		{?char_btns, ActionButtons},
		{?char_spells, Spells}
	],
	lists:foreach(fun({Tab, Val}) ->
		dets_store:store_new(Tab, {Guid, Val}, true),
		ok
	end, DetsValues),
	dets_store:store_new(?char_val, {Guid, Values}, false),

	InitialCharSlotValues = item:init_char_slot_values(),
	dets_store:store_new(?char_items, {Guid, InitialCharSlotValues}, true),

	ok.


update_slot_values(Guid, Values) when is_binary(Values) ->
	dets_store:store(?char_items, {Guid, Values}, true).


update_char_misc(Guid, CharMisc) when is_record(CharMisc, char_misc) ->
	dets_store:store(?char_misc, {Guid, CharMisc}, true).


update_char_move(Guid, CharMove) ->
	dets_store:store(?char_mv, {Guid, CharMove}, true).

update_coords(Guid, X, Y, Z, O, MovementInfo) ->
	CharMv = get_char_move(Guid),
	NewCharMv = CharMv#char_move{x=X, y=Y, z=Z, orient=O, movement_info=MovementInfo},
	dets_store:store(?char_mv, {Guid, NewCharMv}, true).


equip_starting_items(Guid) ->
	Values = get_stored_values(Guid),
	Race = char_values:get(race, Values),
	Class = char_values:get(class, Values),
	Gender = char_values:get(gender, Values),
	StartingItemIds = static_store:lookup_start_outfit(Race, Class, Gender, true),
	lists:foreach(fun(ItemId) ->
		SlotValues = get_slot_values(Guid),
		ok = item:equip_new(ItemId, SlotValues, Guid)
	end, StartingItemIds).


create_action_buttons(ActionButtonData) ->
	Data = init_action_buttons(),
	lists:foldl(fun(ActionButtonDatum, ActionButtons) ->
		insert_action_button(ActionButtonDatum, ActionButtons)
	end, Data, ActionButtonData).


update_action_button(Guid, ActionButtonDatum) ->
	ActionButtons = get_action_buttons(Guid),
	NewActionButtons = insert_action_button(ActionButtonDatum, ActionButtons),
	dets_store:store(?char_btns, {Guid, NewActionButtons}, true).

insert_action_button({Button, Action, Type}, ActionButtons) ->
	% each button is store as 4 bytes
	Offset = Button * 4,
	<<Head:Offset/binary, _?L, Rest/binary>> = ActionButtons,
	NewActionButton = Action bor (Type bsl 24),
	<<Head/binary, NewActionButton?L, Rest/binary>>.

init_action_buttons() ->
	binary:copy(<<0?L>>, ?max_action_buttons).



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


take_damage(Damage, Guid) ->
	OldAmount = player_state:get_value(Guid, health),
	NewAmount = if OldAmount >= Damage ->
			OldAmount - Damage;
		OldAmount < Damage -> 0
	end,
	player_state:set_value(Guid, NewAmount, health).
