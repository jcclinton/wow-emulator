-module(item_values).

-export([get_guid/1]).


get_guid(Values) ->
	object_values:get_uint64_value( 'OBJECT_FIELD_GUID', Values).

