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

-module(item_values_tests).

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
	 {setup, fun init_create/0, fun stop/1, fun create_test/1}}
	].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



init_create() ->
	ItemGuid = 100,
	ItemId = 25, % worn shortsword
	OwnerGuid = 50,
	Guid = ItemGuid bor (0 bsl 24) bor (?highguid_item bsl 48),
	{Guid, ItemId, OwnerGuid}.

init_values() ->
	Values = get_empty_values(),
	Value32 = 16#1010BABA,
	Value64 = 16#EFEF5678ACAC1234,
	Fields = get_fields(),
	{Values, Value32, Value64, Fields}.



stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_test({ItemGuid, ItemId, OwnerGuid}) ->
	Values = item_values:create(ItemGuid, ItemId, OwnerGuid),

	GetGuid = item_values:get_value(object_field_guid, Values),

	GetItemId = item_values:get_value(object_field_entry, Values),

	GetStackCount = item_values:get_value(item_field_stack_count, Values),

	GetOwner = item_values:get_value(item_field_owner, Values),

	GetContained = item_values:get_value(item_field_contained, Values),

	[
		?_assertEqual(ItemGuid, GetGuid),
		?_assertEqual(ItemId, GetItemId),
		?_assertEqual(1, GetStackCount),
		?_assertEqual(OwnerGuid, GetOwner),
		?_assertEqual(ItemGuid, GetContained)
	].




get_set_test({Values, Value32, Value64, Fields}) ->

	% test that any values are getting changed

	WordFields = proplists:get_value(32, Fields),
	QuadFields = proplists:get_value(64, Fields),



	%sets
	{T1, NewValues1, Count1} = lists:foldl(fun(Field, {TestAcc, ValuesAcc, Count}) ->
		NewValuesInner = item_values:set_value(Field, Value64+Count, ValuesAcc),
		TestAccOut = [ ?_assertNotEqual(ValuesAcc, NewValuesInner) | TestAcc],
		{TestAccOut, NewValuesInner, Count+1}
	end, {[], Values, 0}, QuadFields),

	{T2, NewValues, _} = lists:foldl(fun(Field, {TestAcc, ValuesAcc, Count}) ->
		NewValuesInner = item_values:set_value(Field, Value32+Count, ValuesAcc),
		TestAccOut = [ ?_assertNotEqual(ValuesAcc, NewValuesInner) | TestAcc],
		{TestAccOut, NewValuesInner, Count+1}
	end, {T1, NewValues1, Count1}, WordFields),

	%gets
	{T3, Count2} = lists:foldl(fun(Field, {TestAcc, Count}) ->
		Value = item_values:get_value(Field, NewValues),
		TestAccOut = [ ?_assertEqual(Value64+Count, Value) | TestAcc],
		{TestAccOut, Count+1}
	end, {T2, 0}, QuadFields),

	{T4, _} = lists:foldl(fun(Field, {TestAcc, Count}) ->
		Value = item_values:get_value(Field, NewValues),
		TestAccOut = [ ?_assertEqual(Value32+Count, Value) | TestAcc],
		{TestAccOut, Count+1}
	end, {T3, Count2}, WordFields),

	T4.





%%%%%%%%%%%%
% private
get_empty_values() ->
	TotalCount = object_fields:get_total_count(item),
	binary:copy(<<0?L>>, TotalCount).

% put all gets and sets here
% gets have to be in the same order as the sets
get_fields() ->
	[
		{32,
			[object_field_entry, item_field_stack_count]},
		{64,
			[object_field_guid, item_field_owner, item_field_contained]
		}
	].
