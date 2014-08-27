-module(stats).

-export([update_all/1]).

-include("include/items.hrl").
-include("include/binary.hrl").
-include("include/database_records.hrl").







% resets all item modification values and then resets them all
update_all(Guid) ->
	%io:format("UPDATING ALL~n"),
	ItemGuids = item:get_equipped_item_guids(Guid),

	NewPropList = lists:foldl(fun(ItemGuid, Acc) ->
		if ItemGuid > 0 ->
				%io:format("~nitem guid: ~p~n", [ItemGuid]),
				update_one(ItemGuid, Acc);
			ItemGuid == 0 -> []
		end
	end, get_empty_fields(), ItemGuids),

	player_state:set_multiple_values(Guid, NewPropList).




% sets all values for one item
% returns updated char values
update_one(ItemGuid, PrevPropList) ->
	ItemProto = item_data:get_item_proto(ItemGuid),
	IsEquippable = item:is_equippable(ItemProto),

	if IsEquippable ->
			Fields = get_fields(),
			lists:foldl(fun(Field, Acc) ->
				Value = get_item_value(Field, ItemProto),
				if Value > 0 ->
						CurrentValue = proplists:get_value(Field, Acc),
						NextAcc = proplists:delete(Field, Acc),
						NewValue = Value + CurrentValue,
						%io:format("setting ~p from ~p to ~p~n", [Field, CurrentValue, NewValue]),
						[{Field, NewValue} | NextAcc];
						%char_values:set(Field, NewValue, AccValues);
					Value == 0 -> Acc
				end
			end, PrevPropList, Fields);
		not IsEquippable -> []
	end.



%set all stats that are currently modifiable below


get_empty_fields() ->
	[{armor, 0}].

get_fields() ->
	[armor].

get_item_value(armor, ItemProto) -> ItemProto#item_proto.armor.
