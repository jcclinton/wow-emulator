%%   This is a World of Warcraft emulator written in erlang, supporting
%%   client 1.12.x
%%
%%   Copyright (C) 2014  Jamie Clinton <jamieclinton.com>
%%
%%   This program is free software; you can redistribute it and/or modify
%%   it under the terms of the GNU General Public License as published by
%%   the Free Software Foundation; either version 2 of the License, or
%%   (at your option) any later version.
%%
%%   This program is distributed in the hope that it will be useful,
%%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%   GNU General Public License for more details.
%%
%%   You should have received a copy of the GNU General Public License along
%%   with this program; if not, write to the Free Software Foundation, Inc.,
%%   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%
%%   World of Warcraft, and all World of Warcraft or Warcraft art, images,
%%   and lore ande copyrighted by Blizzard Entertainment, Inc.

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
	 {setup, fun init/0, fun stop/1, fun overflow_value_test/1}},
	{"set empty test",
	 {setup, fun init_numbers/0, fun stop/1, fun empty_set_test/1}},
	{"overflow offset test",
	 {setup, fun init_numbers/0, fun stop/1, fun overflow_offset_test/1}},
	{"overflow value test",
	 {setup, fun init_numbers/0, fun stop/1, fun overflow_value_test/1}}
	].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init() ->
	TotalCount = 10,
	EmptyValues = binary:copy(<<0?L>>, TotalCount),
	Index = object_field_guid,
	Value = 5,
	{TotalCount, EmptyValues, Index, Value}.

init_numbers() ->
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

	NewValuesLInt = object_values:set_int32_value(Index, -1, EmptyValues),
	TestSetLongInt = [?_assertNotEqual(NewValuesLInt, EmptyValues)],

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

	NewValueLInt = object_values:get_int32_value(Index, NewValuesLInt),
	TestGetLongInt = [?_assertEqual(-1, NewValueLInt)],

	NewValueQ = object_values:get_uint64_value(Index, NewValuesQ),
	TestGetQuad = [?_assertEqual(NewValueQ, Value)],

	TestSetByte ++ TestSetWord ++ TestSetLong ++ TestSetQuad ++
		TestGetByte ++ TestGetWord ++ TestGetLong ++ TestGetQuad ++
		TestSetLongInt ++ TestGetLongInt.



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

		?_assertThrow(badarg, object_values:set_int32_value(Index, BadLongValue1, EmptyValues)),
		?_assertThrow(badarg, object_values:set_uint32_value(Index, BadLongValue1, EmptyValues)),
		?_assertThrow(badarg, object_values:set_uint32_value(Index, BadLongValue2, EmptyValues)),
		?_assertThrow(badarg, object_values:set_uint64_value(Index, BadQuadValue1, EmptyValues)),
		?_assertThrow(badarg, object_values:set_uint64_value(Index, BadQuadValue2, EmptyValues))
	].
