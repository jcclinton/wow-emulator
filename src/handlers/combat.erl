-module(combat).

-export([set_sheathed/1]).

-include("include/binary.hrl").

set_sheathed(Data) ->
	<<Value?L>> = recv_data:get(payload, Data),
	Guid = recv_data:get(guid, Data),
	Values = char_data:get_values(Guid),
	NewValues = char_values:set_sheathed(Value, Values),
	char_data:update_values(Guid, NewValues),

	ok.
