# rebar3_nova

Rebar3 plugin for scaffolding and managing Nova framework projects.

## Build
```bash
rebar3 compile
```

## Commands provided
```bash
rebar3 new nova myapp        # Create new Nova project
rebar3 nova serve            # Dev server with file watching + hot reload
rebar3 nova routes           # Display routing tree
rebar3 nova gen_controller   # Generate controller module
rebar3 nova gen_resource     # Generate controller + schema + route snippets
rebar3 nova gen_test         # Generate CT test suite
rebar3 nova gen_auth         # Generate email/password auth scaffolding
rebar3 nova openapi          # Generate OpenAPI spec to priv/assets/
rebar3 nova middleware       # Show plugin/middleware chains
rebar3 nova config           # Show Nova configuration
rebar3 nova audit            # Security audit of routes
rebar3 nova release          # Build release (regenerates OpenAPI if schemas exist)
```

## Key modules
- `rebar3_nova.erl` — plugin registration, rebar3 version check
- `rebar3_nova_prv.erl` — base provider (help)
- `rebar3_nova_serve.erl` — dev server with inotify file watching, hot reload via `code:load_binary/3`
- `rebar3_nova_routes.erl` — route tree display with Unicode box-drawing
- `rebar3_nova_gen_controller.erl` — controller scaffolding
- `rebar3_nova_gen_resource.erl` — full resource scaffolding (controller + schema + routes)
- `rebar3_nova_gen_test.erl` — CT test suite generation
- `rebar3_nova_gen_auth.erl` — email/password auth scaffolding (9 files)
- `rebar3_nova_openapi.erl` — OpenAPI 3.0.3 spec + Swagger UI HTML (outputs to priv/assets/)
- `rebar3_nova_middleware.erl` — middleware/plugin chain display
- `rebar3_nova_config.erl` — Nova config display
- `rebar3_nova_audit.erl` — route security audit
- `rebar3_nova_release.erl` — release builder (wraps rebar3 release)
- `rebar3_nova_utils.erl` — shared helpers (app name/dir, ensure_dir, file writing)

## Templates
- `priv/templates/nova/` — Erlang project scaffold
- `priv/templates/nova_lfe/` — LFE project scaffold
- `priv/templates/nova_plugin.template` — plugin module
- `priv/templates/nova_websocket.template` — WebSocket module

## Architecture
Uses rebar3 provider pattern. Each command is a separate provider module in the `nova` namespace. All depend on `{default, compile}`.

## Git workflow
Default branch is `master`. NEVER push directly — always create a PR.
