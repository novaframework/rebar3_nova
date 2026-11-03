# Rebar3 plugin for Nova

A rebar plugin for nova

## Build

    $ rebar3 compile

## Installation

Add the plugin to your rebar config (`~/.config/rebar3/rebar.config`):

    {plugins, [
        {rebar3_nova, {git, "https://github.com/novaframework/rebar3_nova.git", {branch, "master"}}}
    ]}.

For latest stable from hex:

    {plugins, [rebar3_nova]}.

Then just call your plugin directly in an existing application:

    $ rebar3 nova
    ===> Fetching rebar3_nova
    ===> Compiling rebar3_nova

## Commands

### `nova serve` — Auto-reload development server

Starts the application with auto-reload. Source file changes are automatically compiled and hot-loaded. Editing the router re-registers its routes, so new routes work without a restart.

```
$ rebar3 nova serve
```

File watching uses the [fs](https://hex.pm/packages/fs) library. On Linux and the BSDs it needs `inotifywait` from inotify-tools (`apt install inotify-tools`, `dnf install inotify-tools`, `pacman -S inotify-tools`). Without it, serve prints a warning and runs without reloading.

The commands that read routes (`routes`, `audit`, `openapi`, `doctor`, `gen_test`) build the route table with the project's own Nova, which must be 0.18 or later.

### `nova routes` — List compiled routes

Displays the compiled routing tree with methods, paths, and handler modules.

```
$ rebar3 nova routes
```

### `nova gen_controller` — Generate a controller

Scaffolds a controller module with stub functions for CRUD actions.

```
$ rebar3 nova gen_controller --name users
===> Created src/controllers/myapp_users_controller.erl

$ rebar3 nova gen_controller --name users --actions list,show
===> Created src/controllers/myapp_users_controller.erl
```

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--name`, `-n` | yes | — | Controller name |
| `--actions`, `-a` | no | `list,show,create,update,delete` | Comma-separated actions to generate |

Generated functions take the Nova request map and return stub responses:
- `list/1`, `show/1`, `update/1` return `{json, #{~"message" => ~"TODO"}}`
- `create/1` returns `{json, 201, #{}, #{~"message" => ~"TODO"}}`
- `delete/1` returns `{status, 204}`

The output is erlfmt-formatted, so `rebar3 fmt --check` passes right after generating.

If the file already exists, the command skips it with a warning.

### `nova gen_resource` — Generate a full resource

Combines controller generation, JSON schema creation, and prints route snippets to add to your router.

```
$ rebar3 nova gen_resource --name products
===> Created src/controllers/myapp_products_controller.erl
===> Created priv/schemas/product.json
===>
===> Add these routes to your router:
===>   {"/products", fun myapp_products_controller:list/1, #{methods => [get]}},
===>   {"/products/:id", fun myapp_products_controller:show/1, #{methods => [get]}},
===>   {"/products", fun myapp_products_controller:create/1, #{methods => [post], extra_state => #{json_schema => "schemas/product.json"}}},
===>   {"/products/:id", fun myapp_products_controller:update/1, #{methods => [put], extra_state => #{json_schema => "schemas/product.json"}}},
===>   {"/products/:id", fun myapp_products_controller:delete/1, #{methods => [delete]}}
```

**Options:** Same as `gen_controller`.

The JSON schema is placed in `priv/schemas/{singular}.json` as a starting point: a draft-06 schema (the newest draft [nova_json_schemas](https://github.com/novaframework/nova_json_schemas) validates) with a required `name`. The resource name is naively singularized by stripping a trailing "s".

### `nova gen_test` — Generate a Common Test suite

Scaffolds a CT suite with a test case per route of `myapp_{name}_controller`, using `httpc`.

```
$ rebar3 nova gen_test --name users
===> Created test/myapp_users_controller_SUITE.erl
```

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--name`, `-n` | yes | — | Resource name |

The paths, prefixes included, come from the compiled routes; with no routes for the controller yet, the suite assumes the `gen_resource` routes under `/{name}`. The generated suite starts `inets` and the application in `init_per_suite`, reads the port from the `nova` `cowboy_configuration` it runs with, uses `1` for path bindings and sends `{"name": "test"}` as the body, so it passes against the generated controller. Adapt the expectations as you implement the actions.

### `nova gen_auth` - Generate token authentication

Scaffolds email/password authentication with bearer tokens on [nova_auth](https://hex.pm/packages/nova_auth) 0.2 and Kura:

- `src/myapp_auth.erl`: the `nova_auth` config module
- `src/schemas/myapp_user.erl`, `src/schemas/myapp_user_token.erl`: Kura schemas; the `rebar3_kura` compile hook writes the migration
- `src/myapp_accounts.erl`: register, authenticate, issue and revoke tokens
- `src/myapp_auth_security.erl`: `require_bearer/1` for route groups
- `src/controllers/myapp_auth_controller.erl`: `register/1`, `login/1`, `logout/1`, `me/1`
- `src/plugins/myapp_rate_limit.erl`: rate limiting for login and registration
- `test/myapp_auth_SUITE.erl`

```
$ rebar3 nova gen_auth
```

It then prints the deps (`nova_auth`, `kura`, `kura_postgres`), the `rebar3_kura` hook and the routes to add. The project needs a Kura repo named `myapp_repo`, as `rebar3 nova new --kura` creates. Pin `{nova_auth, "~> 0.2.0"}`: the rate-limit plugin sets nova_auth 0.2's `key_fun` option, which later versions reject.

- Put the rate limiter before `nova_request_plugin`, as the printed routes do, so a flood is refused before its body is decoded. It keys on the TCP peer, which is the client only when the node is exposed directly. Behind a reverse proxy every client shares the proxy's address and one bucket: switch to the commented-out `client_ip/1` in the plugin, which reads `x-forwarded-for` only from the proxies in `{trusted_proxies, [...]}` in the app env and takes the right-most address that is not one of them.

### `nova openapi` — Generate OpenAPI 3.0.3 spec

Generates an OpenAPI 3.0.3 JSON specification from compiled controller routes and any JSON schemas found in `priv/schemas/`. Static files, error pages and inline funs are left out.

```
$ rebar3 nova openapi
===> OpenAPI spec written to /path/to/myapp/priv/assets/openapi.json

$ rebar3 nova openapi --prefix /api --output priv/openapi/openapi.json
$ rebar3 nova openapi --title "My API" --api-version 1.0.0
```

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--output`, `-o` | no | `priv/assets/openapi.json` | Output file path |
| `--title`, `-t` | no | app name | API title |
| `--api-version`, `-v` | no | `0.1.0` | API version string |
| `--prefix`, `-p` | no | (every route) | Only document paths under this prefix, e.g. `/api` |

Responses follow the method: `201` for POST, `204` for DELETE and `200` otherwise, plus `400` when the route has a request schema and `404` when the path has parameters. A route's `extra_state => #{json_schema => "schemas/x.json"}`, the key [nova_json_schemas](https://github.com/novaframework/nova_json_schemas) validates request bodies with, becomes the request body schema of POST, PUT and PATCH operations; it is not used to describe responses. Schemas are read from `priv/schemas` and its subdirectories and named by their relative path, so `priv/schemas/v1/user.json` is the component `v1.user`. `$schema` and `$id` are dropped from the component schemas, which OpenAPI 3.0 does not allow.

The default output directory, `priv/assets`, is what the scaffold serves under `/assets`, so the spec is public there; pass `--output` to keep it elsewhere. No Swagger UI page is written: earlier versions wrote a `swagger.html` that loaded Swagger UI from a CDN. Serve a self-hosted copy if you want one.

### `nova middleware` — Show plugin/middleware chains

Displays global plugins and per-route-group plugin chains, preserving the grouping defined in your router's `routes/1` function.

```
$ rebar3 nova middleware

=== Global Plugins ===
  (none)

=== Route Groups (myapp_router) ===

  Group: prefix=/  security=false
  Plugins:
    (inherits global: none)
  Routes:
    GET /heartbeat -> myapp_health_controller:heartbeat
    GET /health -> myapp_health_controller:health
    STATIC /assets/[...] -> assets
    WS /ws -> myapp_ws (websocket)
    STATUS 404 -> myapp_error_controller:not_found

  Group: prefix=/api  security=fun myapp_auth:validate_token/1
  Plugins:
    pre_request: nova_cors_plugin #{allow_origins => <<"*">>}
    pre_request: nova_request_logger #{level => info}
  Routes:
    GET /users -> myapp_users_controller:list
    POST /users -> myapp_users_controller:create
```

Groups that don't specify their own `plugins` key inherit from the global Nova plugin configuration.

### `nova config` — Show Nova configuration

Displays the settings Nova reads from `sys.config`, grouped by the application env each one is read from, with current values or `(default)` when using built-in defaults. `json_lib` and `use_sessions` belong to the bootstrap application; a value set under `nova` instead is pointed out.

```
$ rebar3 nova config

=== Nova Configuration ===

  {nova, [...]}
    bootstrap_application     myapp
    environment               dev
    cowboy_configuration      #{port => 8080}
    plugins                   [] (default)
    use_stacktrace            false (default)
    render_error_pages        true (default)
    use_strict_routing        false (default)
    session_manager           nova_session_ets (default)
    dispatch_backend          persistent_term (default)

  {myapp, [...]}
    json_lib                  thoas (default)
    use_sessions              true (default)
```

Warns if `bootstrap_application` is not set.

### `nova audit` — Audit route security

Checks compiled routes for common security issues and prints findings grouped by severity.

```
$ rebar3 nova audit

=== Security Audit ===

  WARNINGS:
    POST /users (myapp_users_controller) has no security
    PUT /users/{id} (myapp_users_controller) has no security
    DELETE /users/{id} (myapp_users_controller) has no security

  INFO:
    GET /users (myapp_users_controller) has no security
    GET /health (myapp_health_controller) has no security

  Summary: 3 warning(s), 2 info(s)
```

**Rules checked:**
- **Warning**: Mutation methods (POST, PUT, DELETE, PATCH) without a security callback
- **Warning**: Wildcard method (`'_'`) matching all HTTP methods on a route (once per route)
- **Info**: GET routes without a security callback

Static file routes, error pages and websocket handlers are not audited. Findings are sorted by path and method.

### `nova doctor` — Diagnose project health

Runs a suite of read-only checks against the current project and reports findings grouped by section. Each finding has a level (`ok`, `warning`, or `error`). Ok findings are collapsed by default; pass `-v` to expand them.

```
$ rebar3 nova doctor

=== Nova Doctor ===

✅ Toolchain
    (3 ok, run with -v for detail)

✅ Project structure
    (3 ok, run with -v for detail)

⚠️ Configuration    1 warning(s)
    ! json_lib is set under nova
        hint: Nova reads json_lib from the bootstrap application; move it to {myapp, [...]}

✅ Routes
    (55 ok, run with -v for detail)

✅ Dependencies
    (2 ok, run with -v for detail)

⚠️ Security hygiene    1 warning(s)
    ! erl_crash.dump in project root
        hint: Delete it (already gitignored)

✅ Build artifacts
    (2 ok, run with -v for detail)

--
Summary: 67 ok, 2 warning(s), 0 error(s)
```

**Sections:**

| Section | Checks |
|---------|--------|
| `toolchain` | OTP version, rebar3 version, `.tool-versions` or `mise.toml` pinning |
| `project` | `bootstrap_application` set, the router (`router_module` or `<app>_router`) exports `routes/1`, `.app.src` lists `nova` in `applications` |
| `config` | `json_lib` (read from the bootstrap app) is loaded and exports `encode/1` and `decode/1`; `{json_lib, json}` is an error with a Nova whose request decoding expects `{ok, Term}`; `json_lib` set under `nova`; Cowboy port; `session_manager`, `use_sessions`, and the unused `session_backend` key |
| `routes` | Every compiled route resolves to an exported handler (or captured fun); flags duplicate `{method, path}` pairs |
| `deps` | `rebar.lock` committed, active profiles |
| `security` | `erl_crash.dump` and `.env` presence vs `.gitignore` |
| `build` | `_build/default` and app `ebin` exist |

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--only` | no | (all) | Comma-separated section ids to run |
| `--skip` | no | (none) | Comma-separated section ids to skip |
| `-v`, `--verbose` | no | `false` | Expand `ok`-level findings, not just warnings/errors |
| `-s`, `--strict` | no | `false` | Exit with code 1 if any warnings are reported (useful in CI) |

Exit code is `0` when there are no errors. Errors always fail; warnings only fail under `--strict`.

```
$ rebar3 nova doctor --only routes
$ rebar3 nova doctor --skip toolchain,deps -v
$ rebar3 nova doctor --strict
```

### `nova release` — Build a release

Runs `rebar3 as <profile> release`. If `priv/schemas/` exists, regenerates the OpenAPI spec (with the `openapi` defaults) before building.

```
$ rebar3 nova release
===> Regenerating OpenAPI spec
===> Building release with profile 'prod'
===> Release built successfully with profile 'prod'

$ rebar3 nova release --profile staging
```

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--profile`, `-p` | no | `prod` | Release profile to use |
