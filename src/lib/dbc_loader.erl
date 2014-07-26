-module(dbc_loader).

-export([load_all/0]).

-include("include/binary.hrl").
-include("include/database_records.hrl").


load_all() ->
	Dbcs = [{"ItemClass.dbc", fun load_item_class/2}],
	lists:foreach(fun({Filename, Fun}) ->
		load(Filename, Fun)
	end, Dbcs),
	ok.


load(Filename, Fun) ->
	File = get_path() ++ Filename,
	Fd = file_open(File, [read, binary]),
	Header = 16#43424457, %WDBC
	Size = 4,
	SizeOffset = Size * 5,
	<<Header?L, RecordCount?L, FieldCount?L, RecordSize?L, StringSize?L>> = file_pread(Fd, 0, SizeOffset),
	io:format("file: ~p~nheader: ~p~nrecord count: ~p~nfield count: ~p~nrecord size: ~p~nstring size: ~p~n",[Filename, Header, RecordCount, FieldCount, RecordSize, StringSize]),


	RecordTotal = RecordSize * RecordCount,
	DataSize = RecordTotal,

	Data = file_pread(Fd, SizeOffset, DataSize),
	ByteSize = byte_size(Data),
	io:format("data size: ~p~n", [ByteSize]),

	StringOffset = SizeOffset + DataSize,
	Strings = file_pread(Fd, StringOffset, StringSize),

	lists:foldl(fun(_, RestIn) ->
		{Tab, Id, Record, RestOut} = Fun(RestIn, Strings),
		object_store:store_new(Tab, {Id, Record}),
		RestOut
	end, Data, lists:seq(1, RecordCount)),

	file_close(Fd).


lookup_string(Offset, Strings) ->
	<<"hi">>.



	%% specific load functions
load_item_class(Data, Strings) ->
	<<Id?L, _Unk1?L, _Unk2?L, NameOffset?Q, _Unk3?QH, _Unk4?Q, _Flags?L, Rest/binary>> = Data,
	Name = lookup_string(NameOffset, Strings),
	Record = #item_class_store{id=Id, name=Name},
	{item_class_store, Id, Record, Rest}.








% test functions


test_items(Data, RecordCount, RecordSize) ->
	io:format("~n"),
	lists:foldl(fun(I, RestIn) ->
		<<Id?L, Unk1?L, Unk2?L, Name?Q, Unk3?QH, Unk4?Q, Flags?L, RestOut/binary>> = RestIn,
			Size = byte_size(RestIn) - byte_size(RestOut),
			io:format("size: ~p~nid: ~p~nunk1: ~p~nunk2: ~p~nname: ~p~nflag: ~p~n", [Size, Id, Unk1, Unk2, Name, Flags]),


			%io:format("unk1: ~p unk2: ~p unk3: ~p~n", [Unk1, Unk2, Unk3]),
			io:format("~n"),
			RestOut
		%end, Data, lists:seq(1, 2)),
		end, Data, lists:seq(1, RecordCount)),

		%io:format("string: ~p~n", [DataOut]),
		ok.


test_char_start_outfit(Data, RecordCount, RecordSize) ->
	io:format("~n"),
	lists:foldl(fun(I, RestIn) ->
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
			RestOut
		%end, Data, lists:seq(1, 2)),
		end, Data, lists:seq(1, RecordCount)),

		%io:format("string: ~p~n", [DataOut]),
		ok.


get_path() -> "./db/dbcs/DBFilesClient/".



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



test() ->
	%Filename = "CharStartOutfit.dbc",
	Filename = "ItemClass.dbc",
	File = get_path() ++ Filename,
	load(File).


load(File) ->
	Fd = file_open(File, [read, binary]),
	Header = 16#43424457,
	Size = 4,
	SizeOffset = Size * 5,
	<<Header?L, RecordCount?L, FieldCount?L, RecordSize?L, StringSize?L>> = file_pread(Fd, 0, SizeOffset),
	io:format("header: ~p~nrecord count: ~p~nfield count: ~p~nrecord size: ~p~nstring size: ~p~n",[Header, RecordCount, FieldCount, RecordSize, StringSize]),


	RecordTotal = RecordSize * RecordCount,
	%TotalSize = RecordTotal + StringSize,
	DataSize = RecordTotal,

	Data = file_pread(Fd, SizeOffset, DataSize),
	ByteSize = byte_size(Data),
	io:format("data size: ~p~n", [ByteSize]),

	%test_char_start_outfit(Data, RecordCount, RecordSize),
	test_items(Data, RecordCount, RecordSize),

	StringOffset = SizeOffset + DataSize,
	StringsBin = file_pread(Fd, StringOffset, StringSize),
	Strings = binary:split(StringsBin, <<0>>, [global]),
	io:format("strings: ~p~n", [Strings]),

	file_close(Fd).
