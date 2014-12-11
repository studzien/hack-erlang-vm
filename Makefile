start: compile
	erl -pa ebin -eval "application:start(hevm)"

compile:
	./rebar compile
