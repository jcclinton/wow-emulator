-module(player_state_functions).

-export([get_first_empty_inv_slot/1]).
-export([take_damage/2]).

-include("include/character.hrl").


get_first_empty_inv_slot(Values) ->
	FirstSlot = ?inventory_slot_item_start,
	get_first_empty_inv_slot(Values, FirstSlot).

get_first_empty_inv_slot(_, ?inventory_slot_item_end) -> -1;
get_first_empty_inv_slot(Values, Slot) ->
	SlotValue = char_values:get(item, {Slot, Values}),

	if SlotValue == 0 -> Slot;
		SlotValue > 0 ->
			get_first_empty_inv_slot(Values, Slot + 1)
	end.




take_damage(Values, Damage) ->
	OldAmount = char_values:get(health, Values),
	NewAmount = if OldAmount >= Damage ->
			OldAmount - Damage;
		OldAmount < Damage -> 0
	end,
	char_values:set(health, NewAmount, Values).
