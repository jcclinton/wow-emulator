-module(stats).

-export([update_values/2]).

-include("include/items.hrl").
-include("include/binary.hrl").
-include("include/database_records.hrl").


% takes char values and item id or item values
% updates stats on char values using item proto
update_values(CharValues, ItemValues) when is_binary(ItemValues) ->
	ItemId = item_values:get_item_id(ItemValues),
	update_values(CharValues, ItemId);
update_values(CharValues, ItemId) when is_number(ItemId), is_binary(CharValues) ->
	ItemProto = content:lookup_item(ItemId),


	Class = ItemProto#item_proto.class,
	UpdateValues = if Class == ?item_class_weapon ->
			Delay = ItemProto#item_proto.delay,
			DmgMin = ItemProto#item_proto.dmg_min1,
			DmgMax = ItemProto#item_proto.dmg_max1,
			[{delay, Delay}, {max_damage, DmgMax}, {min_damage, DmgMin}];
		Class == ?item_class_armor ->
			Armor = ItemProto#item_proto.armor,
			Block = ItemProto#item_proto.block,
			[{block, Block}, {armor, Armor}]
	end,


	%io:format("stat type1: ~p~n stat value1: ~p~nstat type2: ~p~nstat value2: ~p~n", [StatType1, StatValue1, StatType2, StatValue2]),
	%io:format("armor: ~p~nblock: ~p~n", [Armor, Block]),

	lists:foldl(fun({Type, Value}, AccValues) ->
		if Value == undefined -> AccValues;
			Value /= undefined ->
				io:format("setting ~p with ~p~n", [Type, Value]),
				char_values:set(Type, Value, AccValues)
		end
	end, CharValues, UpdateValues).
		

	%CharValues.
