PROJECT = ev8_require
DIALYZER = dialyzer

all: compile

compile: clean rebar-compile

dependencies:
	./rebar get-deps
	
rebar-compile:
	./rebar compile

clean:
	./rebar clean

console: compile
	erl -pa ../ev8_require -pa ebin/ -pa deps/*/ebin/ -s ev8_require

test: eunit ct

eunit: compile
	./rebar -C rebar.tests.config eunit skip_deps=true

ct: compile
	rm -rf logs
	./rebar -C rebar.tests.config ct skip_deps=true || open logs/index.html

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns
