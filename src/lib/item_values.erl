-module(item_values).

-export([get_guid/1, get_item_id/1]).
-export([set_contained/2, set_owner/2]).


get_guid(Values) ->
	object_values:get_uint64_value( 'OBJECT_FIELD_GUID', Values).

get_item_id(Values) ->
	object_values:get_uint32_value( 'OBJECT_FIELD_ENTRY', Values).


set_owner(Value, Values) ->
	Field = 'ITEM_FIELD_OWNER',
	set_uint64_mark_if_needed(Field, Value, Values).

set_contained(Value, Values) ->
	Field = 'ITEM_FIELD_CONTAINED',
	set_uint64_mark_if_needed(Field, Value, Values).


set_uint64_mark_if_needed(Field, NewValue, Values) ->
	Value = object_values:get_uint64_value(Field, Values),
	if Value /= NewValue ->
			mark_update(Field, Values),
			object_values:set_uint64_value(Field, NewValue, Values);
		true -> Values
	end.

mark_update(_Field, _Values) ->
	% todo implement
	ok.
