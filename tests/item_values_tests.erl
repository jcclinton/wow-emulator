-module(item_values_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/binary.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

object_values_test_() ->
	[{"get set test",
	 {setup, fun init/0, fun stop/1, fun get_set_test/1}},

	{"underflow value test",
	 {setup, fun init_underflow/0, fun stop/1, fun overflow_value_test/1}},
	{"overflow value test",
	 {setup, fun init_overflow/0, fun stop/1, fun overflow_value_test/1}}
	].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



init() ->
	Values = get_empty_values(),
	Value32 = 16#1010BABA,
	Value64 = 16#EFEF5678ACAC1234,
	{Values, Value32, Value64}.

init_overflow() ->
	Values = get_empty_values(),
	Value32 = 16#FFFFFFFF + 1,
	Value64 = 16#FFFFFFFFFFFFFFFF + 1,
	{Values, Value32, Value64}.

init_underflow() ->
	Values = get_empty_values(),
	Value32 = -1,
	Value64 = -1,
	{Values, Value32, Value64}.


stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_set_test({Values, Value32, Value64}) ->

	% test that any values are getting changed

	NewValues1 = item_values:set_guid(Value64, Values),
	NewValues2 = item_values:set_item_id(Value32, NewValues1),
	NewValues3 = item_values:set_owner(Value64, NewValues2),
	NewValues = item_values:set_contained(Value64, NewValues3),

	TestSetNotEq = [
		?_assertNotEqual(Values, NewValues1),
		?_assertNotEqual(NewValues1, NewValues2),
		?_assertNotEqual(NewValues2, NewValues3),
		?_assertNotEqual(NewValues3, NewValues)
	],

	ValueGuid = item_values:get_guid(NewValues),
	ValueItemId = item_values:get_item_id(NewValues),
	ValueOwner = item_values:get_owner(NewValues),
	ValueContained = item_values:get_contained(NewValues),

	TestSetEq = [
		?_assertEqual(Value64, ValueGuid),
		?_assertEqual(Value32, ValueItemId),
		?_assertEqual(Value64, ValueOwner),
		?_assertEqual(Value64, ValueContained)
	],

	TestSetNotEq ++ TestSetEq.



overflow_value_test({Values, Value32, Value64}) ->
	[
		?_assertThrow(badarg, item_values:set_guid(Value64, Values)),
		?_assertThrow(badarg, item_values:set_item_id(Value32, Values)),
		?_assertThrow(badarg, item_values:set_owner(Value64, Values)),
		?_assertThrow(badarg, item_values:set_contained(Value64, Values))
	].


%%%%%%%%%%%%
% private
get_empty_values() ->
	TotalCount = update_fields:fields('ITEM_END'),
	binary:copy(<<0?L>>, TotalCount).
