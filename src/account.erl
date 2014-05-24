-module(account).
-export([create/2, init/0, destroy/0, lookup/1]).

-include("include/database_records.hrl").


init() ->
	ets:new(users, [named_table, set, public]),
	account:create("alice", "password123"),
	ok.

destroy() ->
	ets:delete(users),
	ok.

lookup(I) ->
	Result = ets:lookup(users, I),
	Value = case Result of
		[] -> false;
		[{_, Record}] -> Record
	end,
	io:format("ets lookup result: ~p~n", [Value]),
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
	Result = ets:insert_new(users, {Key, Value}),
	if Result -> ok;
		not Result -> {error, name_taken}
	end;
create(_,_) ->
	throw(badarg).
