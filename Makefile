shell:
	erl -pa ebin/ deps/*/ebin -eval "application:start(gproc)"

resh: clean compile
	@make shell

re: clean compile
	echo "cleaned and compiled"

beam:
	rm ./ebin/*.beam

compile:
	./rebar compile

clean:
	./rebar clean

eunit:
	./rebar eunit
