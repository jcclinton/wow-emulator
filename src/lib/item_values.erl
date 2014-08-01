-module(item_values).

-export([get_guid/1, get_item_id/1]).
-export([get_stack_count/1]).
-export([get_owner/1, get_contained/1]).

-export([set_guid/2, set_item_id/2]).
-export([set_contained/2, set_owner/2]).
-export([set_stack_count/2]).

% gets

get_guid(Values) ->
	object_values:get_uint64_value('OBJECT_FIELD_GUID', Values).

get_item_id(Values) ->
	object_values:get_uint32_value('OBJECT_FIELD_ENTRY', Values).

get_stack_count(Values) ->
	object_values:get_uint32_value( 'ITEM_FIELD_STACK_COUNT', Values).

get_owner(Values) ->
	object_values:get_uint64_value('ITEM_FIELD_OWNER', Values).

get_contained(Values) ->
	object_values:get_uint64_value('ITEM_FIELD_CONTAINED', Values).


% sets

set_stack_count(Value, Values) ->
	object_values:set_uint32_value('ITEM_FIELD_STACK_COUNT', Value, Values).

set_contained(Value, Values) ->
	object_values:set_uint64_value('ITEM_FIELD_CONTAINED', Value, Values).

set_guid(Value, Values) ->
	object_values:set_uint64_value('OBJECT_FIELD_GUID', Value, Values).

set_item_id(Value, Values) ->
	object_values:set_uint32_value('OBJECT_FIELD_ENTRY', Value, Values).

set_owner(Value, Values) ->
	object_values:set_uint64_value('ITEM_FIELD_OWNER', Value, Values).
