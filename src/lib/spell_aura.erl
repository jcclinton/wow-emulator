-module(spell_aura).

-export([add/2]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/spell.hrl").
-include("include/database_records.hrl").


add(Guid, Spell) ->
	SpellId = Spell#spell_store.id,
	Values = char_data:get_values(Guid),
	Slot = 8,
	V1 = char_values:set_aura(Slot, SpellId, Values),
	V2 = char_values:set_aura_flag(Slot, V1),
	V3 = char_values:set_aura_level(Slot, 1, V2),
	NewValues = char_values:set_aura_application(Slot, V3),
	char_data:update_values(Guid, NewValues),

	timer:apply_after(100, spell_aura, set_aura_duration, [Guid, Slot]).


set_aura_duration(Guid, Slot) ->
	OpAtom = smsg_update_aura_duration,
	Time = 1000 * 60 * 30,
	Payload = <<Slot?B, Time?L>>,
	AccountId = char_data:get_account_id(Guid),
	player_controller:send(AccountId, OpAtom, Payload),
	ok.
