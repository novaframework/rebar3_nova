.PHONY: all compile serve setup clean

all: compile

compile:
	@rebar3 compile

serve:
	@rebar3 nova serve

setup:
	docker compose up -d
	@rebar3 compile

clean:
	@rebar3 clean
