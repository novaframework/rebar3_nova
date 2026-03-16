.PHONY: all compile serve setup test clean

all: compile

compile:
	@rebar3 compile

serve: compile
	@rebar3 nova serve

setup:
	docker compose up -d
	@rebar3 compile

test:
	docker compose up -d
	@rebar3 fmt --check
	@rebar3 xref
	@rebar3 dialyzer
	@rebar3 eunit
	@rebar3 ct

clean:
	@rebar3 clean
