shell:
	erl -pa ebin/ deps/*/ebin -eval "application:start(gproc)"

clean:
	./rebar clean

eunit:
	./rebar eunit
