-module(dbc_loader).

-export([load_all/0]).
-export([test/0]).

-include("include/binary.hrl").
-include("include/database_records.hrl").

-define(E, :96?IL). % 3 long words
-define(Ch, :256?IL).


get_path() -> "./db/dbcs/DBFilesClient/".



load_all() ->
	Dbcs = [
		{"ItemClass.dbc", fun load_item_class/2},
		{"Spell.dbc", fun load_spells/2}
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
	<<Header?L, RecordCount?L, _FieldCount?L, RecordSize?L, StringSize?L>> = util:file_pread(Fd, 0, SizeOffset),


	RecordTotal = RecordSize * RecordCount,
	DataSize = RecordTotal,

	Data = util:file_pread(Fd, SizeOffset, DataSize),

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


load_spells(Data, Strings) ->
	%record size is 692
	<<Id?L,
	School?L,
	Category?L,
	_CastUI?L,
	Dispel?L,
	Mechanic?L,
	Attributes?L,
	AttributesEx?L,
	AttributesEx2?L,
	AttributesEx3?L,
	AttributesEx4?L,
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
	ProcChance?L,
	ProcCharges?L,
	MaxLevel?L,
	BaseLevel?L,
	SpellLevel?L,
	DurationIndex?L,
	PowerType?L,
	ManaCost?L,
	ManaCostPerLevel?L,
	ManaPerSecond?L,
	ManaPerSecondPerLevel?L,
	RangeIndex?L,
	Speed?f,
	ModalNextSpell?L,
	StackAmount?L,
	Totem?Q, % array
	Reagent?QQ, % array, signed
	ReagentCount?QQ, % array
	EquippedItemClass?L,
	EquippedItemSubClassMask?L,
	EquippedItemInventoryTypeMask?L,
	Effect?E, % array
	EffectDieSides?E, %array, signed
	EffectBaseDice?E, %array
	EffectDicePerLevel?E, % array, floats
	EffectRealPointsPerLevel?E, % array, floats
	EffectBasePoints?E, % array, signed
	EffectMechanic?E, % array
	EffectImplicitTargetA?E, % array
	EffectImplicitTargetB?E, % array
	EffectRadiusIndex?E, % array
	EffectApplyAuraName?E, % array
	EffectAmplitude?E, % array
	EffectMultipleValue?E, % array, floats
	EffectChainTarget?E, % array
	EffectItemType?E, % array
	EffectMiscValue?E, % array, signed
	EffectTriggerSpell?E, % array
	EffectPointsPerComboPoint?E, % array, floats
	SpellVisual?L,
	_SpellVisual2?L,
	SpellIconId?L,
	ActiveIconId?L,
	_SpellPriority?L,
	SpellName?Ch,
	_SpellNameFlag?L,
	Rank?Ch,
	_RankFlags?L,
	_Description?Ch,
	_DescriptionFlags?L,
	_ToolTip?Ch,
	_ToolTipFlags?L,
	ManaCostPercentage?L,
	StartRecoveryCategory?L,
	StartRecoveryTime?L,
	MaxTargetLevel?L,
	SpellFamilyName?L,
	SpellFamilyFlags?Q,
	MaxAffectedTargets?L,
	DmgClass?L,
	PreventionType?L,
	_StanceBarOrder?L,
	DmgMultiplier?E, % array, floats
	_MinFactionId?L,
	_MinReputation?L,
	_RequiredAuraVision?L,

	Rest/binary>> = Data,

	Name = lookup_string(SpellName, Strings),
	RankName = lookup_string(Rank, Strings),
	Record = #spell_store{
		id=Id,
		school=School,
		category=Category,
		dispel=Dispel,
		mechanic=Mechanic,
		attributes=Attributes,
		attributes_ex=AttributesEx,
		attributes_ex2=AttributesEx2,
		attributes_ex3=AttributesEx3,
		attributes_ex4=AttributesEx4,
		stances=Stances,
		stances_not=StancesNot,
		targets=Targets,
		target_creature_type=TargetCreatureType,
		requires_spell_focus=RequiresSpellFocus,
		caster_aura_state=CasterAuraState,
		target_aura_state=TargetAuraState,
		casting_time_index=CastingTimeIndex,
		recovery_time=RecoveryTime,
		category_recovery_time=CategoryRecoveryTime,
		interrupt_flags=InterruptFlags,
		aura_interrupt_flags=AuraInterruptFlags,
		channel_interrupt_flags=ChannelInterruptFlags,
		proc_flags=ProcFlags,
		proc_chance=ProcChance,
		proc_charges=ProcCharges,
		max_level=MaxLevel,
		base_level=BaseLevel,
		spell_level=SpellLevel,
		duration_index=DurationIndex,
		power_type=PowerType,
		mana_cost=ManaCost,
		mana_cost_perlevel=ManaCostPerLevel,
		mana_per_second=ManaPerSecond,
		mana_per_second_per_level=ManaPerSecondPerLevel,
		range_index=RangeIndex,
		speed=Speed,
		modal_next_spell=ModalNextSpell,
		stack_amount=StackAmount,
		totem=Totem,
		reagent=Reagent,
		reagent_count=ReagentCount,
		equipped_item_class=EquippedItemClass,
		equipped_item_sub_class_mask=EquippedItemSubClassMask,
		equipped_item_inventory_type_mask=EquippedItemInventoryTypeMask,
		effect=Effect,
		effect_die_sides=EffectDieSides,
		effect_base_dice=EffectBaseDice,
		effect_dice_per_level=EffectDicePerLevel,
		effect_real_points_per_level=EffectRealPointsPerLevel,
		effect_base_points=EffectBasePoints,
		effect_mechanic=EffectMechanic,
		effect_implicit_target_a=EffectImplicitTargetA,
		effect_implicit_target_b=EffectImplicitTargetB,
		effect_radius_index=EffectRadiusIndex,
		effect_apply_aura_name=EffectApplyAuraName,
		effect_amplitude=EffectAmplitude,
		effect_multiple_value=EffectMultipleValue,
		effect_chain_target=EffectChainTarget,
		effect_item_type=EffectItemType,
		effect_misc_value=EffectMiscValue,
		effect_trigger_spell=EffectTriggerSpell,
		effect_points_per_combo_point=EffectPointsPerComboPoint,
		spell_visual=SpellVisual,
		spell_icon_id=SpellIconId,
		active_icon_id=ActiveIconId,
		spell_name=Name,
		rank=RankName,
		mana_cost_percentage=ManaCostPercentage,
		start_recovery_category=StartRecoveryCategory,
		start_recovery_time=StartRecoveryTime,
		max_target_level=MaxTargetLevel,
		spell_family_name=SpellFamilyName,
		spell_family_flags=SpellFamilyFlags,
		max_affected_targets=MaxAffectedTargets,
		dmg_class=DmgClass,
		prevention_type=PreventionType,
		dmg_multiplier=DmgMultiplier
	},
	{spell_store, Id, Record, Rest}.








% test functions



test_char_start_outfit(RestIn, _Strings) ->
		<<Id?L,
			Race?B,
			Class?B,
			Gender?B,
			_Unk?B,
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
