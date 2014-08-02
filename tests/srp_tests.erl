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
