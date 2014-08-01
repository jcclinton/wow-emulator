-module(account).
-export([create/2, init/0, destroy/0, lookup/1]).
-export([create_dummy_accounts/0]).

-include("include/database_records.hrl").

-define(acc, accounts).


init() ->
	dets_store:open(?acc),
	ok.

create_dummy_accounts() ->
	account:create("alice", "password123"),
	account:create("alice2", "password123").

destroy() ->
	dets_store:close(?acc),
	ok.

lookup(I) ->
	Result = dets_store:lookup(?acc, I),
	Value = case Result of
		[] -> false;
		[{_, Record}] -> Record
	end,
	Value.

create(Name, Password) when is_list(Name), is_list(Password), length(Name) > 0, length(Password) > 0 ->
	BinName = list_to_binary(Name),
	BinPassword = list_to_binary(Password),
	Salt = srp:generatePrivate(),
	DerivedKey = srp:getDerivedKey(BinName, BinPassword, Salt),
	Generator = srp:getGenerator(),
	Prime = srp:getPrime(),
	Verifier = srp:getVerifier(Generator, Prime, DerivedKey),
	Value = #account{name=BinName, salt=Salt, verifier=Verifier},
	Key = srp:normalize(BinName),
	%io:format("storing ~p~n", [Key]),
	Result = dets_store:store_new(?acc, {Key, Value}),
	if Result -> ok;
		not Result -> {error, name_taken}
	end;
create(_,_) ->
	throw(badarg).
