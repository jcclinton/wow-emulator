-module(object_values_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/binary.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

object_values_test_() ->
	[{"set empty test",
	 {setup, fun init/0, fun stop/1, fun empty_set_test/1}},
	{"overflow offset test",
	 {setup, fun init/0, fun stop/1, fun overflow_offset_test/1}},
	{"overflow value test",
	 {setup, fun init/0, fun stop/1, fun overflow_value_test/1}}
	].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init() ->
	TotalCount = 10,
	EmptyValues = binary:copy(<<0?L>>, TotalCount),
	Index = 1,
	Value = 5,
	{TotalCount, EmptyValues, Index, Value}.


stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_set_test({_TotalCount, EmptyValues, Index, Value}) ->

	% test that any values are getting changed

	NewValuesByte0 = object_values:set_byte_value(Index, Value, EmptyValues, 0),
	NewValuesByte1 = object_values:set_byte_value(Index, Value, EmptyValues, 1),
	NewValuesByte2 = object_values:set_byte_value(Index, Value, EmptyValues, 2),
	NewValuesByte3 = object_values:set_byte_value(Index, Value, EmptyValues, 3),
	TestSetByte = [
		?_assertNotEqual(NewValuesByte0, EmptyValues),
		?_assertNotEqual(NewValuesByte1, EmptyValues),
		?_assertNotEqual(NewValuesByte2, EmptyValues),
		?_assertNotEqual(NewValuesByte3, EmptyValues)
	],


	NewValuesWord0 = object_values:set_uint16_value(Index, Value, EmptyValues, 0),
	NewValuesWord1 = object_values:set_uint16_value(Index, Value, EmptyValues, 1),
	TestSetWord = [
		?_assertNotEqual(NewValuesWord0, EmptyValues),
		?_assertNotEqual(NewValuesWord1, EmptyValues)
	],

	NewValuesL = object_values:set_uint32_value(Index, Value, EmptyValues),
	TestSetLong = [?_assertNotEqual(NewValuesL, EmptyValues)],

	NewValuesQ = object_values:set_uint64_value(Index, Value, EmptyValues),
	TestSetQuad = [?_assertNotEqual(NewValuesQ, EmptyValues)],


	% test that values out are correct

	NewValueByte0 = object_values:get_byte_value(Index, NewValuesByte0, 0),
	NewValueByte1 = object_values:get_byte_value(Index, NewValuesByte1, 1),
	NewValueByte2 = object_values:get_byte_value(Index, NewValuesByte2, 2),
	NewValueByte3 = object_values:get_byte_value(Index, NewValuesByte3, 3),
	TestGetByte = [
		?_assertEqual(NewValueByte0, Value),
		?_assertEqual(NewValueByte1, Value),
		?_assertEqual(NewValueByte2, Value),
		?_assertEqual(NewValueByte3, Value)
	],

	NewValueWord0 = object_values:get_uint16_value(Index, NewValuesWord0, 0),
	NewValueWord1 = object_values:get_uint16_value(Index, NewValuesWord1, 1),
	TestGetWord = [
		?_assertEqual(NewValueWord0, Value),
		?_assertEqual(NewValueWord1, Value)
	],

	NewValueL = object_values:get_uint32_value(Index, NewValuesL),
	TestGetLong = [?_assertEqual(NewValueL, Value)],

	NewValueQ = object_values:get_uint64_value(Index, NewValuesQ),
	TestGetQuad = [?_assertEqual(NewValueQ, Value)],

	TestSetByte ++ TestSetWord ++ TestSetLong ++ TestSetQuad ++ 
		TestGetByte ++ TestGetWord ++ TestGetLong ++ TestGetQuad.



overflow_offset_test({_TotalCount, EmptyValues, Index, Value}) ->
	TestByteOffset1 = [ ?_assertThrow(badarg, object_values:set_byte_value(Index, Value, EmptyValues, 4)) ],
	TestByteOffset2 = [ ?_assertThrow(badarg, object_values:set_byte_value(Index, Value, EmptyValues, -1)) ],
	TestWordOffset1 = [ ?_assertThrow(badarg, object_values:set_uint16_value(Index, Value, EmptyValues, 2)) ],
	TestWordOffset2 = [ ?_assertThrow(badarg, object_values:set_uint16_value(Index, Value, EmptyValues, -1)) ],

	TestByteOffset1 ++ TestByteOffset2 ++ TestWordOffset1  ++ TestWordOffset2.


overflow_value_test({_TotalCount, EmptyValues, Index, _Value}) ->
	BadByteValue1 = 16#FF + 1,
	BadByteValue2 = -1,

	BadWordValue1 = 16#FFFF + 1,
	BadWordValue2 = -1,

	BadLongValue1 = 16#FFFFFFFF + 1,
	BadLongValue2 = -1,

	BadQuadValue1 = 16#FFFFFFFFFFFFFFFF + 1,
	BadQuadValue2 = -1,

	[
		?_assertThrow(badarg, object_values:set_byte_value(Index, BadByteValue1, EmptyValues, 1)),
		?_assertThrow(badarg, object_values:set_byte_value(Index, BadByteValue2, EmptyValues, 1)),
		?_assertThrow(badarg, object_values:set_uint16_value(Index, BadWordValue1, EmptyValues, 1)),
		?_assertThrow(badarg, object_values:set_uint16_value(Index, BadWordValue2, EmptyValues, 1)),

		?_assertThrow(badarg, object_values:set_uint32_value(Index, BadLongValue1, EmptyValues)),
		?_assertThrow(badarg, object_values:set_uint32_value(Index, BadLongValue2, EmptyValues)),
		?_assertThrow(badarg, object_values:set_uint64_value(Index, BadQuadValue1, EmptyValues)),
		?_assertThrow(badarg, object_values:set_uint64_value(Index, BadQuadValue2, EmptyValues))
	].
