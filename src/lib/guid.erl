-module(guid).

-export([pack/1, format/3]).

-include("include/binary.hrl").


		% first byte is a mask that tells you the size of the guid
		% eg for a 3 byte guid,
		% 7 = (1 << 0) bor (1 << 1) bor (1 << 2)
		% objects can be a maximum up to 8 bytes
pack(Guid) when is_integer(Guid), Guid > 0 ->
	%<<7?B, Guid?G>>.
	% set GuidBin to big endian to make it easier to recurse
	GuidBin = <<Guid?QB>>,
	{Count, Mask} = pack(GuidBin, 8, 16#FF),
	Size = Count * 8,
	<<Mask?B, Guid:Size/unsigned-little-integer>>.
pack(<<0?B, Rest/binary>>, Count, Mask) ->
	pack(Rest, Count-1, Mask bsr 1);
pack(_, Count, Mask) -> {Count, Mask}.


format(HighGuid, Entry, LowGuid) ->
	LowGuid bor (Entry bsl 24) bor (HighGuid bsl 48).
