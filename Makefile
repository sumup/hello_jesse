.PHONY: test eunit deps

PROJECT=hello_jesse

REBAR=rebar
DIALYZER=dialyzer

all: deps compile

deps:
	$(REBAR) get-deps

compile:
ifeq ($(NODEPS),true)
	$(REBAR) compile skip_deps=true
else
	$(REBAR) compile
endif

clean:
	$(REBAR) clean

test: all
	rm -rf .eunit
	$(REBAR) eunit skip_deps=true

eunit:
ifeq ($(SUITES),)
	$(REBAR) eunit skip_deps=true
else
	$(REBAR) eunit skip_deps=true suites=$(SUITES)
endif

build-plt:
	$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl inets crypto public_key ssl deps/*

dialyze:
	$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns
