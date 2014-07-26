-module(dbc_loader).

-export([load_all/0]).
-export([test/0]).

-include("include/binary.hrl").
-include("include/database_records.hrl").

-define(E, :96?IL). % long
-define(Ef, :96/float-little).
-define(Ch, :256?IL).


get_path() -> "./db/dbcs/DBFilesClient/".



load_all() ->
	Dbcs = [
		{"ItemClass.dbc", fun load_item_class/2}
	],
	lists:foreach(fun({Filename, Fun}) ->
		load(Filename, Fun)
	end, Dbcs),
	ok.


load(Filename, Fun) ->
	File = get_path() ++ Filename,
	Fd = util:file_open(File, [read, binary]),
	Header = 16#43424457, %WDBC
	Size = 4,
	SizeOffset = Size * 5,
	<<Header?L, RecordCount?L, FieldCount?L, RecordSize?L, StringSize?L>> = util:file_pread(Fd, 0, SizeOffset),
	io:format("file: ~p~nheader: ~p~nrecord count: ~p~nfield count: ~p~nrecord size: ~p~nstring size: ~p~n",[Filename, Header, RecordCount, FieldCount, RecordSize, StringSize]),


	RecordTotal = RecordSize * RecordCount,
	DataSize = RecordTotal,

	Data = util:file_pread(Fd, SizeOffset, DataSize),
	ByteSize = byte_size(Data),
	io:format("data size: ~p~n", [ByteSize]),

	StringOffset = SizeOffset + DataSize,
	Strings = util:file_pread(Fd, StringOffset, StringSize),

	lists:foldl(fun(_, RestIn) ->
		{Tab, Id, Record, RestOut} = Fun(RestIn, Strings),
		static_store:store_new(Tab, {Id, Record}),
		RestOut
	end, Data, lists:seq(1, RecordCount)),

	util:file_close(Fd).




lookup_string(Offset, Strings) ->
	StringSize = byte_size(Strings),
	if StringSize > Offset ->
			<<_:Offset/binary, Rest/binary>> = Strings,
			[String|_] = binary:split(Rest, <<0>>),
			String;
		StringSize =< Offset -> <<"">>
	end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% specific load functions

load_item_class(Data, Strings) ->
	<<Id?L, _Unk1?L, _Unk2?L, NameOffset?Q, _Unk3?QH, _Unk4?Q, _Flags?L, Rest/binary>> = Data,
	Name = lookup_string(NameOffset, Strings),
	Record = #item_class_store{id=Id, name=Name},
	{item_class_store, Id, Record, Rest}.








% test functions



test_char_start_outfit(RestIn, Strings) ->
		<<Id?L,
			Race?B,
			Class?B,
			Gender?B,
			Unk?B,
			ItemIds:48/binary,
			ItemDisplayIds:48/binary,
			ItemInvSlots:48/binary,
			RestOut/binary>> = RestIn,

			ByteSizeIn =byte_size(RestIn) - byte_size(RestOut),
			io:format("size: ~p~nid: ~p~nRace: ~p~nClass: ~p~nGender: ~p~n", [ByteSizeIn, Id, Race, Class, Gender]),

			lists:foldl(fun(I, <<ItemId?L, RestIds/binary>>) ->
				io:format("item id ~p: ~p~n", [I, ItemId]),
				RestIds
			end, ItemIds, lists:seq(1, 12)),

			lists:foldl(fun(I, <<ItemDisplayId?L, RestIds/binary>>) ->
				io:format("item display id ~p: ~p~n", [I, ItemDisplayId]),
				RestIds
			end, ItemDisplayIds, lists:seq(1, 12)),

			lists:foldl(fun(I, <<ItemInvId?L, RestIds/binary>>) ->
				io:format("item inv slot ~p: ~p~n", [I, ItemInvId]),
				RestIds
			end, ItemInvSlots, lists:seq(1, 12)),

			%io:format("unk1: ~p unk2: ~p unk3: ~p~n", [Unk1, Unk2, Unk3]),
			io:format("~n"),
			RestOut.


load_spells(Data, Strings) ->
	%record size is 692
	<<Id?L,
	School?L,
	Category?L,
	CastUI?L,
	Dispel?L,
	Mechanic?L,
	Attr?L,
	AttrEx?L,
	AttrEx2?L,
	AttrEx3?L,
	AttrEx4?L,
	Stances?L,
	StancesNot?L,
	Targets?L,
	TargetCreatureType?L,
	RequiresSpellFocus?L,
	CasterAuraState?L,
	TargetAuraState?L,
	CastingTimeIndex?L,
	RecoveryTime?L,
	CategoryRecoveryTime?L,
	InterruptFlags?L,
	AuraInterruptFlags?L,
	ChannelInterruptFlags?L,
	ProcFlags?L,
	ProcChances?L,
	ProcCharges?L,
	MaxLevel?L,
	BaseLevel?L,
	SpellLevel?L,
	DurationIndex?L,
	PowerType?L,
	ManaCost?L,
	ManaCostPerLevel?L,
	ManaCostPerSecond?L,
	ManaCostPerSecondPerLevel?L,
	RangeIndex?L,
	Speed?f,
	ModalNextSpell?L,
	StackAmount?L,
	Totem?Q,
	Reagent?QQ,
	ReagentCount?QQ,
	EquippedItemClass?L,
	EquippedItemSubClassMask?L,
	EquippedItemInventoryTypeMask?L,
	Effect?E,
	EffectDiesSides?E,
	EffectBaseDice?E,
	EffectDicePerLevel?Ef,
	EffectRealPointsPerLevel?Ef,
	EffectBasePoints?E,
	EffectMechanic?E,
	EffectImplicitTargetA?E,
	EffectImplicitTargetB?E,
	EffectRadiusIndex?E,
	EffectApplyAuraName?E,
	EffectAmplitude?E,
	EffectMultipleValue?Ef,
	EffectChainTarget?E,
	EffectItemType?E,
	EffectMiscValue?E,
	EffectTriggerSpell?E,
	EffectPointsPerComboPoint?Ef,
	SpellVisual?L,
	SpellVisual2?L,
	SpellIconId?L,
	ActiveIconId?L,
	SpellPriority?L,
	SpellName?Ch,
	SpellNameFlag?L,
	Rank?Ch,
	RankFlags?L,
	Description?Ch,
	DescriptionFlags?L,
	ToolTip?Ch,
	ToolTipFlags?L,
	ManaCostPercentages?L,
	StartRecoveryCategory?L,
	StartRecoveryTime?L,
	MaxTargetLevel?L,
	SpellFamilyName?L,
	SpellFamilyFlags?Q,
	MaxAffectedTargets?L,
	DmgClass?L,
	PreventionType?L,
	StanceBarOrder?L,
	DmgMultiplier?Ef,
	MinFactionId?L,
	MinReputation?L,
	RequiredAuraVision?L,

	Rest/binary>> = Data,
	Name = lookup_string(SpellName, Strings),
	%Record = #item_class_store{id=Id, name=Name},
	ByteSize =byte_size(Data) - byte_size(Rest),
	io:format("size: ~p~nid: ~p~nname offset: ~p~nname: ~p~n", [ByteSize, Id, SpellName, Name]),
	io:format("~n"),
	Rest.


% old unused functions, can be used to test new stores


test() ->
	Filename = "Spell.dbc",
	Fun = fun load_spells/2,
	load_test(Filename, Fun).


load_test(Filename, Fun) ->
	File = get_path() ++ Filename,
	Fd = util:file_open(File, [read, binary]),
	Header = 16#43424457, %WDBC
	Size = 4,
	SizeOffset = Size * 5,
	<<Header?L, RecordCount?L, FieldCount?L, RecordSize?L, StringSize?L>> = util:file_pread(Fd, 0, SizeOffset),
	io:format("file: ~p~nheader: ~p~nrecord count: ~p~nfield count: ~p~nrecord size: ~p~nstring size: ~p~n",[Filename, Header, RecordCount, FieldCount, RecordSize, StringSize]),


	RecordTotal = RecordSize * RecordCount,
	DataSize = RecordTotal,

	Data = util:file_pread(Fd, SizeOffset, DataSize),
	ByteSize = byte_size(Data),
	io:format("data size: ~p~n", [ByteSize]),

	StringOffset = SizeOffset + DataSize,
	Strings = util:file_pread(Fd, StringOffset, StringSize),
	io:format("~n"),

	lists:foldl(fun(_, RestIn) ->
		Fun(RestIn, Strings)
	%end, Data, lists:seq(1, RecordCount)),
	end, Data, lists:seq(1, 10)),

	util:file_close(Fd).
