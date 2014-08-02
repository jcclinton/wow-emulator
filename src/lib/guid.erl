-module(guid).

-export([pack/1, format/3]).

-include("include/binary.hrl").


		% first byte is a mask that tells you the size of the guid
		% eg for a 3 byte guid,
		% 7 = (1 << 0) bor (1 << 1) bor (1 << 2)
		% objects can be a maximum up to 8 bytes
pack(Guid) ->
	%<<7?B, Guid?G>>.
	<<16#FF?B, Guid?Q>>.


format(HighGuid, Entry, LowGuid) ->
	LowGuid bor (Entry bsl 24) bor (HighGuid bsl 48).
