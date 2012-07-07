REBAR=rebar

all: app

app: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean
	rm -f erl_crash.dmp

run:
	cp config/client-config.json priv/mchat/.
	erl -pa ebin deps/*/ebin -args_file config/vm.args -config config/sys.config -s mchat_app

