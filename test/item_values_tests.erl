-module(item_values_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/binary.hrl").
-include("include/types.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

object_values_test_() ->
	[{"get set test",
	 {setup, fun init_values/0, fun stop/1, fun get_set_test/1}},

	{"underflow value test",
	 {setup, fun init_underflow/0, fun stop/1, fun throw_test/1}},
	{"overflow value test",
	 {setup, fun init_overflow/0, fun stop/1, fun throw_test/1}},

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
	Funs = get_funs(),
	{Values, Value32, Value64, Funs}.

init_overflow() ->
	Values = get_empty_values(),
	Value32 = 16#FFFFFFFF + 1,
	Value64 = 16#FFFFFFFFFFFFFFFF + 1,
	Funs = get_funs(),
	{Values, Value32, Value64, Funs}.

init_underflow() ->
	Values = get_empty_values(),
	Value32 = -1,
	Value64 = -1,
	Funs = get_funs(),
	{Values, Value32, Value64, Funs}.


stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_test({ItemGuid, ItemId, OwnerGuid}) ->
	Values = item_values:create(ItemGuid, ItemId, OwnerGuid),

	GetGuid = item_values:get_guid(Values),

	GetItemId = item_values:get_item_id(Values),

	GetStackCount = item_values:get_stack_count(Values),

	GetOwner = item_values:get_owner(Values),

	GetContained = item_values:get_contained(Values),

	[
		?_assertEqual(ItemGuid, GetGuid),
		?_assertEqual(ItemId, GetItemId),
		?_assertEqual(1, GetStackCount),
		?_assertEqual(OwnerGuid, GetOwner),
		?_assertEqual(ItemGuid, GetContained)
	].




get_set_test({Values, Value32, Value64, Funs}) ->

	% test that any values are getting changed

	SetFuns = proplists:get_value(sets, Funs),
	SetFuns32 = proplists:get_value(32, SetFuns),
	SetFuns64 = proplists:get_value(64, SetFuns),

	GetFuns = proplists:get_value(gets, Funs),
	GetFuns32 = proplists:get_value(32, GetFuns),
	GetFuns64 = proplists:get_value(64, GetFuns),

	%sets
	{T1, NewValues1, Count1} = lists:foldl(fun(SetFun, {TestAcc, ValuesAcc, Count}) ->
		NewValuesInner = item_values:SetFun(Value64+Count, ValuesAcc),
		TestAccOut = [ ?_assertNotEqual(ValuesAcc, NewValuesInner) | TestAcc],
		{TestAccOut, NewValuesInner, Count+1}
	end, {[], Values, 0}, SetFuns64),

	{T2, NewValues, _} = lists:foldl(fun(SetFun, {TestAcc, ValuesAcc, Count}) ->
		NewValuesInner = item_values:SetFun(Value32+Count, ValuesAcc),
		TestAccOut = [ ?_assertNotEqual(ValuesAcc, NewValuesInner) | TestAcc],
		{TestAccOut, NewValuesInner, Count+1}
	end, {T1, NewValues1, Count1}, SetFuns32),

	%gets
	{T3, Count2} = lists:foldl(fun(GetFun, {TestAcc, Count}) ->
		Value = item_values:GetFun(NewValues),
		TestAccOut = [ ?_assertEqual(Value64+Count, Value) | TestAcc],
		{TestAccOut, Count+1}
	end, {T2, 0}, GetFuns64),

	{T4, _} = lists:foldl(fun(GetFun, {TestAcc, Count}) ->
		Value = item_values:GetFun(NewValues),
		TestAccOut = [ ?_assertEqual(Value32+Count, Value) | TestAcc],
		{TestAccOut, Count+1}
	end, {T3, Count2}, GetFuns32),

	T4.



throw_test({Values, Value32, Value64, Funs}) ->
	SetFuns = proplists:get_value(sets, Funs),
	SetFuns32 = proplists:get_value(32, SetFuns),
	SetFuns64 = proplists:get_value(64, SetFuns),

	T1 = lists:foldl(fun(SetFun, Acc) ->
		[?_assertThrow(badarg, item_values:SetFun(Value64, Values) ) | Acc]
	end, [], SetFuns64),

	lists:foldl(fun(SetFun, Acc) ->
		[?_assertThrow(badarg, item_values:SetFun(Value32, Values) ) | Acc]
	end, T1, SetFuns32).


%%%%%%%%%%%%
% private
get_empty_values() ->
	TotalCount = update_fields:fields('ITEM_END'),
	binary:copy(<<0?L>>, TotalCount).

% put all gets and sets here
% gets have to be in the same order as the sets
get_funs() ->
	[
		{sets,
			[
				{32,
					[set_item_id, set_stack_count]},
				{64,
					[set_guid, set_owner, set_contained]
				}
			]
		},
		{gets,
			[
				{32,
					[get_item_id, get_stack_count]},
				{64,
					[get_guid, get_owner, get_contained]
				}
			]
		}
	].
