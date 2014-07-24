-module(char_values).


-export([set_anim_state/2]).
-export([get_guid/1]).



set_anim_state(AnimState, Values) ->
	Field = 'UNIT_FIELD_BYTES_1',
	mark_update(Field, Values),
	object_values:set_byte_value(Field, AnimState, Values, 0).


get_guid(Values) ->
	Field = 'OBJECT_FIELD_GUID',
	mark_update(Field, Values),
	object_values:get_uint32_value(Field, Values).



%% private

mark_update(Field, Values) ->
	Guid = get_guid(Values),
	AccountId = char_data:get_account_id(Guid),
	player_character:mark_update(AccountId, Field),
	Mask = char_data:get_mask(Guid),
	NewMask = update_mask:set_bit(Field, Mask),
	char_data:store_mask(Guid, NewMask),
	ok.
	%use key to update bit mask
	% set update to run every 30ms or so, if its been marked
