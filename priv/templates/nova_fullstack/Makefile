.PHONY: all compile serve setup assets clean

all: compile

compile:
	@rebar3 compile
	@$(MAKE) assets

assets:
	@mkdir -p priv/static/assets/js
	@cp _build/default/lib/arizona_core/priv/static/assets/js/*.min.js priv/static/assets/js/ 2>/dev/null || true

serve: compile
	@rebar3 nova serve

setup:
	docker compose up -d
	@rebar3 compile
	@$(MAKE) assets
	@rebar3 kura migrate

clean:
	@rebar3 clean
	@rm -rf priv/static/assets/js/arizona*.min.js priv/static/assets/js/index.min.js
