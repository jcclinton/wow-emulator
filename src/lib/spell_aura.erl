-module(spell_aura).

-export([add/2]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/spell.hrl").
-include("include/database_records.hrl").


add(Guid, Spell) ->
	SpellId = Spell#spell_store.id,
	Slot = 12,
	Level = 1,

	PropList = [
		{aura, {Slot, SpellId}},
		{aura_flag, Slot},
		{aura_level, {Slot, Level}},
		{aura_application, Slot}
	],
	player_state:set_multiple_values(Guid, PropList),


	timer:apply_after(100, spell_aura, set_aura_duration, [Guid, Slot]).


set_aura_duration(Guid, Slot) ->
	OpAtom = smsg_update_aura_duration,
	Time = 1000 * 60 * 30,
	Payload = <<Slot?B, Time?L>>,
	AccountId = char_data:get_account_id(Guid),
	player_controller:send(AccountId, OpAtom, Payload),
	ok.
