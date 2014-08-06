-module(stats).

-export([update_values/2]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


% takes char values and item id or item values
% updates stats on char values using item proto
update_values(CharValues, ItemValues) when is_binary(ItemValues) ->
	ItemId = item_values:get_item_id(ItemValues),
	update_values(CharValues, ItemId);
update_values(CharValues, ItemId) when is_number(ItemId), is_binary(CharValues) ->
	ItemProto = content:lookup_item(ItemId),
	StatType1 = ItemProto#item_proto.stat_type1,
	StatValue1 = ItemProto#item_proto.stat_value1,
	StatType2 = ItemProto#item_proto.stat_type2,
	StatValue2 = ItemProto#item_proto.stat_value2,

	Armor = ItemProto#item_proto.armor,
	Block = ItemProto#item_proto.block,

	%io:format("stat type1: ~p~n stat value1: ~p~nstat type2: ~p~nstat value2: ~p~n", [StatType1, StatValue1, StatType2, StatValue2]),
	io:format("armor: ~p~nblock: ~p~n", [Armor, Block]),

	CharValues.
