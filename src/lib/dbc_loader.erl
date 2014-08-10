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
		{"Spell.dbc", fun load_spells/2},
		{"CharStartOutfit.dbc", fun load_char_start_outfit/2}
	],
	lists:foreach(fun({Filename, Fun}) ->
		catch load(Filename, Fun)
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

	Effect1?L,
	Effect2?L,
	Effect3?L,
	EffectDieSides1?SL,
	EffectDieSides2?SL,
	EffectDieSides3?SL,
	EffectBaseDice1?L,
	EffectBaseDice2?L,
	EffectBaseDice3?L,
	EffectDicePerLevel1?f,
	EffectDicePerLevel2?f,
	EffectDicePerLevel3?f,
	EffectRealPointsPerLevel1?f,
	EffectRealPointsPerLevel2?f,
	EffectRealPointsPerLevel3?f,
	EffectBasePoints1?SL,
	EffectBasePoints2?SL,
	EffectBasePoints3?SL,
	EffectMechanic1?L,
	EffectMechanic2?L,
	EffectMechanic3?L,
	EffectImplicitTargetA1?L,
	EffectImplicitTargetA2?L,
	EffectImplicitTargetA3?L,
	EffectImplicitTargetB1?L,
	EffectImplicitTargetB2?L,
	EffectImplicitTargetB3?L,
	EffectRadiusIndex1?L,
	EffectRadiusIndex2?L,
	EffectRadiusIndex3?L,
	EffectApplyAuraName1?L,
	EffectApplyAuraName2?L,
	EffectApplyAuraName3?L,
	EffectAmplitude1?L,
	EffectAmplitude2?L,
	EffectAmplitude3?L,
	EffectMultipleValue1?f,
	EffectMultipleValue2?f,
	EffectMultipleValue3?f,
	EffectChainTarget1?L,
	EffectChainTarget2?L,
	EffectChainTarget3?L,
	EffectItemType1?L,
	EffectItemType2?L,
	EffectItemType3?L,
	EffectMiscValue1?SL,
	EffectMiscValue2?SL,
	EffectMiscValue3?SL,
	EffectTriggerSpell1?L,
	EffectTriggerSpell2?L,
	EffectTriggerSpell3?L,
	EffectPointsPerComboPoint1?f,
	EffectPointsPerComboPoint2?f,
	EffectPointsPerComboPoint3?f,

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
	DmgMultiplier1?f,
	DmgMultiplier2?f,
	DmgMultiplier3?f,
	_MinFactionId?L,
	_MinReputation?L,
	_RequiredAuraVision?L,

	Rest/binary>> = Data,



	Effect3Rec = #spell_effect{
		effect=Effect3,
		effect_die_sides=EffectDieSides3,
		effect_base_dice=EffectBaseDice3,
		effect_dice_per_level=EffectDicePerLevel3,
		effect_real_points_per_level=EffectRealPointsPerLevel3,
		effect_base_points=EffectBasePoints3,
		effect_mechanic=EffectMechanic3,
		effect_implicit_target_a=EffectImplicitTargetA3,
		effect_implicit_target_b=EffectImplicitTargetB3,
		effect_radius_index=EffectRadiusIndex3,
		effect_apply_aura_name=EffectApplyAuraName3,
		effect_amplitude=EffectAmplitude3,
		effect_multiple_value=EffectMultipleValue3,
		effect_chain_target=EffectChainTarget3,
		effect_item_type=EffectItemType3,
		effect_misc_value=EffectMiscValue3,
		effect_trigger_spell=EffectTriggerSpell3,
		effect_points_per_combo_point=EffectPointsPerComboPoint3
	},
	Effect2Rec = #spell_effect{
		effect=Effect2,
		effect_die_sides=EffectDieSides2,
		effect_base_dice=EffectBaseDice2,
		effect_dice_per_level=EffectDicePerLevel2,
		effect_real_points_per_level=EffectRealPointsPerLevel2,
		effect_base_points=EffectBasePoints2,
		effect_mechanic=EffectMechanic2,
		effect_implicit_target_a=EffectImplicitTargetA2,
		effect_implicit_target_b=EffectImplicitTargetB2,
		effect_radius_index=EffectRadiusIndex2,
		effect_apply_aura_name=EffectApplyAuraName2,
		effect_amplitude=EffectAmplitude2,
		effect_multiple_value=EffectMultipleValue2,
		effect_chain_target=EffectChainTarget2,
		effect_item_type=EffectItemType2,
		effect_misc_value=EffectMiscValue2,
		effect_trigger_spell=EffectTriggerSpell2,
		effect_points_per_combo_point=EffectPointsPerComboPoint2
	},
	Effect1Rec = #spell_effect{
		effect=Effect1,
		effect_die_sides=EffectDieSides1,
		effect_base_dice=EffectBaseDice1,
		effect_dice_per_level=EffectDicePerLevel1,
		effect_real_points_per_level=EffectRealPointsPerLevel1,
		effect_base_points=EffectBasePoints1,
		effect_mechanic=EffectMechanic1,
		effect_implicit_target_a=EffectImplicitTargetA1,
		effect_implicit_target_b=EffectImplicitTargetB1,
		effect_radius_index=EffectRadiusIndex1,
		effect_apply_aura_name=EffectApplyAuraName1,
		effect_amplitude=EffectAmplitude1,
		effect_multiple_value=EffectMultipleValue1,
		effect_chain_target=EffectChainTarget1,
		effect_item_type=EffectItemType1,
		effect_misc_value=EffectMiscValue1,
		effect_trigger_spell=EffectTriggerSpell1,
		effect_points_per_combo_point=EffectPointsPerComboPoint1
	},

	Effects = lists:filter(fun(EffRec) ->
		EffRec#spell_effect.effect /= 0
	end, [Effect1Rec, Effect2Rec, Effect3Rec]),

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

		effects=Effects,

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
		dmg_multiplier=[DmgMultiplier1, DmgMultiplier2, DmgMultiplier3]
	},
	{spell_store, Id, Record, Rest}.








% test functions



load_char_start_outfit(RestIn, _Strings) ->
		<<_Id?L,
			Race?B,
			Class?B,
			Gender?B,
			_Unk?B,
			ItemIdsIn:48/binary,
			_ItemDisplayIds:48/binary,
			_ItemInvSlots:48/binary,
			RestOut/binary>> = RestIn,

			%ByteSizeIn =byte_size(RestIn) - byte_size(RestOut),
			%io:format("size: ~p~nid: ~p~nRace: ~p~nClass: ~p~nGender: ~p~n", [ByteSizeIn, Id, Race, Class, Gender]),

			{_, ItemIdsOutRev} = lists:foldl(fun(_I, {<<ItemId?L, RestIds/binary>>, Acc}) ->
				%io:format("item id ~p: ~p~n", [I, ItemId]),
				{RestIds, [ItemId| Acc]}
			end, {ItemIdsIn, []}, lists:seq(1, 12)),
			ItemIdsOut = lists:reverse(ItemIdsOutRev),
			%Record = #char_start_outfit_store{race=Race, class=Class, gender=Gender, item_ids=ItemIdsOut},

			%lists:foldl(fun(I, <<ItemDisplayId?L, RestIds/binary>>) ->
				%io:format("item display id ~p: ~p~n", [I, ItemDisplayId]),
				%RestIds
			%end, ItemDisplayIds, lists:seq(1, 12)),

			%lists:foldl(fun(I, <<ItemInvId?L, RestIds/binary>>) ->
				%io:format("item inv slot ~p: ~p~n", [I, ItemInvId]),
				%RestIds
			%end, ItemInvSlots, lists:seq(1, 12)),

			%io:format("unk1: ~p unk2: ~p unk3: ~p~n", [Unk1, Unk2, Unk3]),
			%io:format("~n"),
			{char_start_outfit_store, {Race, Class, Gender}, ItemIdsOut, RestOut}.


% old unused functions, can be used to test new stores


test() ->
	Filename = "CharStartOutfit.dbc",
	Fun = fun load_char_start_outfit/2,
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
