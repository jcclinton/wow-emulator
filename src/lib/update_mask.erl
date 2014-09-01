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

-module(update_mask).

-export([set_bits/3, set_bit/2, get_bit/2]).
-export([empty/1]).

-include("include/binary.hrl").


set_bit(Field, Mask) when is_atom(Field) ->
	Index = update_fields:fields(Field),
	set_bit(Index, Mask);
set_bit(Index, Mask) ->
	set_bit(Index, 1, Mask).

set_bit(Index, Bit, Mask) ->
	MaskIndex = Index bsr 3,
	LowIndex = Index band 16#7,
	BitValue = Bit bsl LowIndex,
	ByteSize = MaskIndex,
	<<Head:ByteSize/binary, OldValue?B, Tail/binary>> = Mask,
	NewValue = OldValue bor BitValue,
	<<Head/binary, NewValue?B, Tail/binary>>.

set_bits(Count, EmptyMask, Values) ->
	lists:foldl(fun(Index, Mask) ->
		Value = object_values:get_uint32_value(Index, Values),
		Bit = if Value > 0 -> 1;
			true -> 0
		end,
		set_bit(Index, Bit, Mask)
	end, EmptyMask, lists:seq(0, Count)).


get_bit(Mask, Index) ->
			MaskIndex = Index bsr 3,
			LowIndex = Index band 16#7,
			Value = 1 bsl LowIndex,
			ByteSize = MaskIndex,
			<<_Head:ByteSize/binary, OldValue?B, _Tail/binary>> = Mask,
			NewValue = OldValue band Value,
			NewValue > 0.


empty(Type) when is_atom(Type) ->
	TotalCount = update_fields:get_total_count(Type),
	update_mask:empty(TotalCount - 1);
empty(Count) ->
	Blocks = (Count + 31) div 32,
	binary:copy(<<0?L>>, Blocks).
