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

-module(char_values_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/binary.hrl").
-include("include/types.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

object_values_test_() ->
	[
	{"get set test",
	 {setup, fun init_values/0, fun stop/1, fun get_set_test/1}},

	{"create values test",
	 {setup, fun empty/0, fun stop/1, fun create_test/1}}
	].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



empty() ->
	Values = char_values:get_empty_values(),
	{Values}.

init_values() ->
	Values = char_values:get_empty_values(),
	Value32 = 16#1010BABA,
	Value64 = 16#EFEF5678ACAC1234,
	Fields = get_fields(),
	{Values, Value32, Value64, Fields}.



stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_test({Values}) ->
	GetGuid = char_values:get_value(object_field_guid, Values),

	GetValue = char_values:get_value(player_duel_arbiter, Values),

	IsBin = is_binary(Values),
	GreaterThanZero = byte_size(Values) > 0,

	[
		?_assertEqual(true, IsBin),
		?_assertEqual(true, GreaterThanZero),
		?_assertEqual(0, GetGuid),
		?_assertEqual(0, GetValue)
	].




get_set_test({Values, Value32, Value64, Fields}) ->

	% test that any values are getting changed

	WordFields = proplists:get_value(32, Fields),
	QuadFields = proplists:get_value(64, Fields),



	%sets
	{T1, NewValues1, Count1} = lists:foldl(fun(Field, {TestAcc, ValuesAcc, Count}) ->
		{NewValuesInner, Indices} = char_values:set_value(Field, Value64+Count, ValuesAcc),
		TestAccOut1 = [ ?_assertNotEqual(ValuesAcc, NewValuesInner) | TestAcc],
		Index = object_fields:fields(Field),
		IndicesSize = length(Indices),
		TestAccOut2 = [ ?_assertEqual([Index, Index+1], Indices) | TestAccOut1],
		TestAccOut = [ ?_assertEqual(2, IndicesSize) | TestAccOut2],
		{TestAccOut, NewValuesInner, Count+1}
	end, {[], Values, 0}, QuadFields),

	{T2, NewValues, _} = lists:foldl(fun(Field, {TestAcc, ValuesAcc, Count}) ->
		{NewValuesInner, Indices} = char_values:set_value(Field, Value32+Count, ValuesAcc),
		TestAccOut1 = [ ?_assertNotEqual(ValuesAcc, NewValuesInner) | TestAcc],
		Index = object_fields:fields(Field),
		TestAccOut2 = [ ?_assertEqual([Index], Indices) | TestAccOut1],
		IndicesSize = length(Indices),
		TestAccOut = [ ?_assertEqual(1, IndicesSize) | TestAccOut2],
		{TestAccOut, NewValuesInner, Count+1}
	end, {T1, NewValues1, Count1}, WordFields),

	%gets
	{T3, Count2} = lists:foldl(fun(Field, {TestAcc, Count}) ->
		Value = char_values:get_value(Field, NewValues),
		TestAccOut = [ ?_assertEqual(Value64+Count, Value) | TestAcc],
		{TestAccOut, Count+1}
	end, {T2, 0}, QuadFields),

	{T4, _} = lists:foldl(fun(Field, {TestAcc, Count}) ->
		Value = char_values:get_value(Field, NewValues),
		TestAccOut = [ ?_assertEqual(Value32+Count, Value) | TestAcc],
		{TestAccOut, Count+1}
	end, {T3, Count2}, WordFields),

	T4.





%%%%%%%%%%%%
% private

% put all gets and sets here
% gets have to be in the same order as the sets
get_fields() ->
	[
		{32,
			[object_field_entry, player_flags]},
		{64,
			[object_field_guid, player_duel_arbiter, player_field_inv_slot_head]
		}
	].
