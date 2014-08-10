-module(stats).

-export([update_all/1]).

-include("include/items.hrl").
-include("include/binary.hrl").
-include("include/database_records.hrl").





unset_values(Values) ->
	Fields = get_empty_fields(),
	lists:foldl(fun({Type, Value}, AccValues) ->
		char_values:set(Type, Value, AccValues)
	end, Values, Fields).



% resets all item modification values and then resets them all
update_all(Guid) ->
	Values = char_data:get_values(Guid),
	BaseValues = unset_values(Values),

	ItemGuids = item:get_equipped_item_guids(Guid),

	NewValues = lists:foldl(fun(ItemGuid, AccValues) ->
		if ItemGuid > 0 ->
				update_one(ItemGuid, AccValues);
			ItemGuid == 0 -> AccValues
		end
	end, BaseValues, ItemGuids),

	char_data:update_values(Guid, NewValues).




% sets all values for one item
% returns updated char values
update_one(ItemGuid, CharValues) ->
	ItemProto = item_data:get_item_proto(ItemGuid),
	IsEquippable = item:is_equippable(ItemProto),
	if IsEquippable ->
			Fields = get_fields(),
			lists:foldl(fun(Type, AccValues) ->
				Value = get_item_value(Type, ItemProto),
				CurrentValue = char_values:get(Type, AccValues),
				if Value > 0 ->
						NewValue = Value + CurrentValue,
						char_values:set(Type, NewValue, AccValues);
					Value == 0 -> AccValues
				end
			end, CharValues, Fields);
		not IsEquippable -> CharValues
	end.



%set all stats that are currently modifiable below


get_empty_fields() ->
	[{armor, 0}].

get_fields() ->
	[armor].

get_item_value(armor, ItemProto) -> ItemProto#item_proto.armor.
