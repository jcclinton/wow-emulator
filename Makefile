shell:
	erl -pa ebin/ deps/*/ebin -eval "application:start(gproc)"

beam:
	rm ./ebin/*.beam

compile:
	./rebar compile

clean:
	./rebar clean

eunit:
	./rebar eunit
