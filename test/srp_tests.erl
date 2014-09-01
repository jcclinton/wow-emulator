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

-module(srp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/binary.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

object_values_test_() ->
	[{"session key test",
	 {setup, fun init_srp/0, fun stop/1, fun run_sess_key_test/1}},
	{"key length test",
	 {setup, fun init_srp/0, fun stop/1, fun key_length_test/1}}
	].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init_srp() ->

	Prime = srp:getPrime(),
	Generator = srp:getGenerator(),

	UBin = <<"alice">>,
	PwBin = <<"password123">>,


	Salt = srp:generatePrivate(),
	DerivedKey = srp:getDerivedKey(UBin, PwBin, Salt),
	Verifier = srp:getVerifier(Generator, Prime, DerivedKey),

	{Generator, Prime, DerivedKey, Verifier}.


stop(_SetupData) ->
	ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

key_length_test({Generator, Prime, _DerivedKey, Verifier}) ->
	LoopSize = get_loop_size(),
	BitSize = bit_size(srp:getPrime()),

	lists:foldl(fun(_, Tests) ->
		{ClientPublic, ClientPrivate} = srp:getClientPublicPrivate(Generator, Prime),
		{ServerPublic, ServerPrivate} = srp:getServerPublicPrivate(Generator, Prime, Verifier),
		Keys = [ServerPublic, ServerPrivate, ClientPublic, ClientPrivate],

		T = lists:foldl(fun(Key, TestsInner) ->
			[ ?_assertEqual(BitSize, bit_size(Key)) | TestsInner]
		end, [], Keys),
		T ++ Tests
	end, [], lists:seq(1, LoopSize)).


run_sess_key_test({Generator, Prime, DerivedKey, Verifier}) ->
	LoopSize = get_loop_size(),

	lists:foldl(fun(_, Tests) ->
		{ClientPublic, ClientPrivate} = srp:getClientPublicPrivate(Generator, Prime),
		{ServerPublic, ServerPrivate} = srp:getServerPublicPrivate(Generator, Prime, Verifier),

		ServerKey = srp:computeServerKey(ServerPrivate, ClientPublic, ServerPublic, Prime, Verifier),
		ClientKey = srp:computeClientKey(ClientPrivate, ServerPublic, ClientPublic, Generator, Prime, DerivedKey),

		T = ?_assertEqual(ServerKey, ClientKey),
		[T | Tests]
	end, [], lists:seq(1, LoopSize)).







%%%%%%%%%%%%%%%%%%%%
%% private helpers

get_loop_size() -> 20.
