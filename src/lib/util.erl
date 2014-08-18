-module(util).

-export([has_flag/2]).
-export([extract_string/1]).
-export([game_time/0, game_speed/0]).
-export([file_pread/3, file_open/2, file_close/1]).
-export([reg_proc/2, get_pid/2, build_pid_key/2]).

-export([bin_test/0]).

-include("include/binary.hrl").


bin_test() ->
	EmptyBin = char_values:get_empty_values(),
	ShortValues = [
		{'OBJECT_FIELD_GUID', uint32, 100},
		{'OBJECT_FIELD_TYPE', float, 100.0},
		{'OBJECT_FIELD_ENTRY', int32, 100}
	],
	Values = [
		{'OBJECT_FIELD_GUID', uint32, 100},
		{'OBJECT_FIELD_TYPE', float, 100.0},
		{'OBJECT_FIELD_ENTRY', int32, 100},
		{'OBJECT_FIELD_SCALE_X', uint32, 100},
		{'OBJECT_FIELD_PADDING', float, 100.0},
		{'UNIT_FIELD_CHARM', uint32, 100},
		{'UNIT_FIELD_SUMMON', uint32, 100},
		{'UNIT_FIELD_CHARMEDBY', int32, -100},
		{'UNIT_FIELD_SUMMONEDBY', float, 98.19291},
		{'UNIT_FIELD_CREATEDBY', uint32,100},
		{'UNIT_FIELD_TARGET', int32, -100},
		{'UNIT_FIELD_PERSUADED', float, -98.68787},
		{'UNIT_FIELD_CHANNEL_OBJECT', uint32,100},
		{'UNIT_FIELD_HEALTH', int32, -100},
		{'UNIT_FIELD_POWER1', float, -98.68787},
		{'UNIT_FIELD_POWER2', uint32,100},
		{'UNIT_FIELD_POWER3', int32, -100},
		{'UNIT_FIELD_POWER4', float, -98.68787},
		{'UNIT_FIELD_POWER5', uint32,100},
		{'UNIT_FIELD_MAXHEALTH', int32, -100},
		{'UNIT_FIELD_MAXPOWER1', float, -98.68787},
		{'UNIT_FIELD_MAXPOWER2', uint32,100},
		{'UNIT_FIELD_MAXPOWER3', int32, -100},
		{'UNIT_FIELD_MAXPOWER4', float, -98.68787},
		{'UNIT_FIELD_MAXPOWER5', uint32,100},
		{'UNIT_FIELD_LEVEL', int32, -100},
		{'UNIT_FIELD_FACTIONTEMPLATE', float, -98.68787},
		{'UNIT_FIELD_BYTES_0', uint32,100},
		{'UNIT_VIRTUAL_ITEM_SLOT_DISPLAY', int32, -100},
		{'UNIT_VIRTUAL_ITEM_SLOT_DISPLAY_01', float, -98.68787},
		{'UNIT_VIRTUAL_ITEM_SLOT_DISPLAY_02', uint32,100},
		{'UNIT_VIRTUAL_ITEM_INFO', int32, -100},
		{'UNIT_VIRTUAL_ITEM_INFO_01', float, -98.68787},
		{'UNIT_VIRTUAL_ITEM_INFO_02', uint32,100},
		{'UNIT_VIRTUAL_ITEM_INFO_03', int32, -100},
		{'UNIT_VIRTUAL_ITEM_INFO_04', float, -98.68787},
		{'UNIT_VIRTUAL_ITEM_INFO_05', uint32,100},
		{'UNIT_FIELD_FLAGS', int32, -100},
		{'UNIT_FIELD_AURA', float, -98.68787},
		{'UNIT_FIELD_AURA_LAST', uint32,100},
		{'UNIT_FIELD_AURAFLAGS', int32, -100},
		{'UNIT_FIELD_AURAFLAGS_01', float, -98.68787},
		{'UNIT_FIELD_AURAFLAGS_02', uint32,100},
		{'UNIT_FIELD_AURAFLAGS_03', int32, -100},
		{'UNIT_FIELD_AURAFLAGS_04', float, -98.68787},
		{'UNIT_FIELD_AURAFLAGS_05', uint32,100},
		{'UNIT_FIELD_AURALEVELS', int32, -100},
		{'UNIT_FIELD_AURALEVELS_LAST', float, -98.68787},
		{'UNIT_FIELD_AURAAPPLICATIONS', uint32,100},
		{'UNIT_FIELD_AURAAPPLICATIONS_LAST', int32, -100},
		{'UNIT_FIELD_AURASTATE', float, -98.68787},
		{'UNIT_FIELD_BASEATTACKTIME', uint32,100},
		{'UNIT_FIELD_OFFHANDATTACKTIME', int32, -100},
		{'UNIT_FIELD_RANGEDATTACKTIME', float, -98.68787},
		{'UNIT_FIELD_BOUNDINGRADIUS', uint32,100},
		{'UNIT_FIELD_COMBATREACH', int32, -100},
		{'UNIT_FIELD_DISPLAYID', float, -98.68787},
		{'UNIT_FIELD_NATIVEDISPLAYID', uint32,100},
		{'UNIT_FIELD_MOUNTDISPLAYID', int32, -100},
		{'UNIT_FIELD_MINDAMAGE', float, -98.68787},
		{'UNIT_FIELD_MAXDAMAGE', uint32,100},
		{'UNIT_FIELD_MINOFFHANDDAMAGE', int32, -100},
		{'UNIT_FIELD_MAXOFFHANDDAMAGE', float, -98.68787},
		{'UNIT_FIELD_BYTES_1', uint32,100},
		{'UNIT_FIELD_PETNUMBER', int32, -100},
		{'UNIT_FIELD_PET_NAME_TIMESTAMP', float, -98.68787},
		{'UNIT_FIELD_PETEXPERIENCE', uint32,100},
		{'UNIT_FIELD_PETNEXTLEVELEXP', int32, -100},
		{'UNIT_DYNAMIC_FLAGS', float, -98.68787},
		{'UNIT_CHANNEL_SPELL', uint32,100},
		{'UNIT_MOD_CAST_SPEED', int32, -100},
		{'UNIT_CREATED_BY_SPELL', float, -98.68787},
		{'UNIT_NPC_FLAGS', uint32,100},
		{'UNIT_NPC_EMOTESTATE', int32, -100},
		{'UNIT_TRAINING_POINTS', float, -98.68787},
		{'UNIT_FIELD_STAT0', uint32,100},
		{'UNIT_FIELD_STAT1', int32, -100},
		{'UNIT_FIELD_STAT2', float, -98.68787},
		{'UNIT_FIELD_STAT3', uint32,100},
		{'UNIT_FIELD_STAT4', int32, -100},
		{'UNIT_FIELD_RESISTANCES', float, -98.68787},
		{'UNIT_FIELD_RESISTANCES_01', uint32,100},
		{'UNIT_FIELD_RESISTANCES_02', int32, -100},
		{'UNIT_FIELD_RESISTANCES_03', float, -98.68787},
		{'UNIT_FIELD_RESISTANCES_04', uint32,100},
		{'UNIT_FIELD_RESISTANCES_05', int32, -100},
		{'UNIT_FIELD_RESISTANCES_06', float, -98.68787},
		{'UNIT_FIELD_BASE_MANA', uint32,100},
		{'UNIT_FIELD_BASE_HEALTH', int32, -100},
		{'UNIT_FIELD_BYTES_2', float, -98.68787},
		{'UNIT_FIELD_ATTACK_POWER', uint32,100},
		{'UNIT_FIELD_ATTACK_POWER_MODS', int32, -100},
		{'UNIT_FIELD_ATTACK_POWER_MULTIPLIER', float, -98.68787},
		{'UNIT_FIELD_RANGED_ATTACK_POWER', uint32,100},
		{'UNIT_FIELD_RANGED_ATTACK_POWER_MODS', int32, -100},
		{'UNIT_FIELD_RANGED_ATTACK_POWER_MULTIPLIER', float, -98.68787},
		{'UNIT_FIELD_MINRANGEDDAMAGE', uint32,100},
		{'UNIT_FIELD_MAXRANGEDDAMAGE', int32, -100},
		{'UNIT_FIELD_POWER_COST_MODIFIER', float, -98.68787},
		{'UNIT_FIELD_POWER_COST_MODIFIER_01', uint32,100},
		{'UNIT_FIELD_POWER_COST_MODIFIER_02', int32, -100},
		{'UNIT_FIELD_POWER_COST_MODIFIER_03', float, -98.68787},
		{'UNIT_FIELD_POWER_COST_MODIFIER_04', uint32,100},
		{'UNIT_FIELD_POWER_COST_MODIFIER_05', int32, -100},
		{'UNIT_FIELD_POWER_COST_MODIFIER_06', float, -98.68787},
		{'UNIT_FIELD_POWER_COST_MULTIPLIER', uint32,100},
		{'UNIT_FIELD_POWER_COST_MULTIPLIER_01', int32, -100},
		{'UNIT_FIELD_POWER_COST_MULTIPLIER_02', float, -98.68787},
		{'UNIT_FIELD_POWER_COST_MULTIPLIER_03', uint32,100},
		{'UNIT_FIELD_POWER_COST_MULTIPLIER_04', int32, -100},
		{'UNIT_FIELD_POWER_COST_MULTIPLIER_05', float, -98.68787},
		{'UNIT_FIELD_POWER_COST_MULTIPLIER_06', uint32,100},
		{'UNIT_FIELD_PADDING', int32, -100}

	],
	Inserts = Values,
	T1 = now(),
	run_values_test(Inserts, EmptyBin),
	T2 = now(),
	run_recurse_test(Inserts, EmptyBin),
	T3 = now(),
	Diff1 = timer:now_diff(T2, T1),
	Diff2 = timer:now_diff(T3, T2),
	io:format("values: ~p~n recurse: ~p~n", [Diff1, Diff2]),
	ok.


run_values_test(ValuesData, Values) ->
	lists:foldl(fun({Field, Type, Value}, Acc) ->
		object_values:set_key_values({Field, Value, Type}, Acc)
	end, Values, ValuesData).

run_recurse_test(Inserts, Values) ->
	SortedInserts = lists:sort(fun({FieldA, _, _}, {FieldB, _, _}) ->
		IndexA = update_fields:fields(FieldA),
		IndexB = update_fields:fields(FieldB),
		IndexA =< IndexB
	end, Inserts),
	run_recurse_test(SortedInserts, Values, <<>>, 0).
run_recurse_test([], <<>>, Acc, _) -> Acc;
run_recurse_test([], <<Word?L, Values/binary>>, Acc, Count) ->
	run_recurse_test([], Values, <<Acc/binary, Word?L>>, Count+1);
run_recurse_test([{Field, Type, Value}|Inserts]=AllInserts, <<Word?L, Values/binary>>, Acc, Count) ->
	Index = update_fields:fields(Field),
	if Index == Count ->
			NewAcc = get_new_bin(Acc, Value, Type),
			run_recurse_test(Inserts, Values, NewAcc, Count+1);
		Index /= Count ->
			run_recurse_test(AllInserts, Values, <<Acc/binary, Word?L>>, Count+1)
	end.


get_new_bin(Acc, Value, Type) ->
	case Type of
		uint32 ->
			<<Acc/binary, Value?L>>;
		float ->
			<<Acc/binary, Value?f>>;
		int32 ->
			<<Acc/binary, Value?SL>>
	end.






reg_proc(Name, Id) ->
	Key = build_pid_key(Name, Id),
	gproc:reg({n, l, Key}, none).

get_pid(Name, Id) ->
	Key = build_pid_key(Name, Id),
	gproc:lookup_pid({n, l, Key}).

build_pid_key(Name, Id) ->
	{Name, Id}.



has_flag(Flags, Flag) ->
	Flags band Flag /= 0.

% pulls out data until first zero byte
% then returns string plus rest of bin
extract_string(Bin) ->
	extract_string(Bin, <<>>).
extract_string(<<0?B, Rest/binary>>, Str) -> {Str, Rest};
extract_string(<<Char?B, Rest/binary>>, Str) ->
	extract_string(Rest, <<Str/binary, Char?B>>).


game_speed() ->
	0.01666667.

game_time() ->
    {Y, Mo, Dm} = erlang:date(),
    {H, Mi, _} = erlang:time(),
    Dw = calendar:day_of_the_week(Y, Mo, Dm),
    GameTime = (((((Mi band 16#3F) bor 
                   (H*64 band 16#7C0)) bor 
                   (Dw*2048 band 16#3800)) bor 
                   ((Dm - 1)*16384 band 16#FC000)) bor 
                   ((Mo - 1)*1048576 band 16#F00000)) bor 
                   ((Y - 2000)*16777216 band 16#1F000000),
    GameTime.



file_pread(Fd, Offset, Size) ->
	case file:pread(Fd, Offset, Size) of
		{error, Error} -> throw(Error);
		{ok, Result} -> Result
	end.

file_open(Filename, Options) ->
	case file:open(Filename, Options) of
		{error, Error} -> throw(Error);
		{ok, Fd} -> Fd
	end.

% wrapper around file:close to simplify code
file_close(Fd) ->
	case file:close(Fd) of
		{error, Error} -> throw(Error);
		ok -> ok
	end.
