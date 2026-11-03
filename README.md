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
===> Add these route groups to your router:
===>   #{prefix => "",
===>     routes => [
===>         {"/products", fun myapp_products_controller:list/1, #{methods => [get]}},
===>         {"/products/:id", fun myapp_products_controller:show/1, #{methods => [get]}}
===>     ]},
===>   #{prefix => "",
===>     %% Placeholder: require_bearer/1 and myapp_body_limit come from
===>     %% `rebar3 nova gen_auth`; use your own if you have them.
===>     security => fun myapp_auth_security:require_bearer/1,
===>     %% Plugins run before security, so the group sets its own instead of
===>     %% inheriting a global body decoder that reads any size of body.
===>     plugins => [
===>         {pre_request, myapp_body_limit, #{}},
===>         {pre_request, nova_request_plugin, #{decode_json_body => true}},
===>         {pre_request, nova_json_schemas, #{}}
===>     ],
===>     routes => [
===>         {"/products", fun myapp_products_controller:create/1, #{methods => [post], extra_state => #{json_schema => "schemas/product.json"}}},
===>         {"/products/:id", fun myapp_products_controller:update/1, #{methods => [put], extra_state => #{json_schema => "schemas/product.json"}}},
===>         {"/products/:id", fun myapp_products_controller:delete/1, #{methods => [delete]}}
===>     ]}
```

Create, update and delete come in a group of their own behind a `security` function, so pasting the snippet does not make the writes public. Until you replace the placeholder or run `gen_auth`, those routes fail closed. The group lists its own plugins, because Nova runs pre-request plugins before security: an inherited global body decoder would read a body of any size from a request without a token. A group's plugins replace the global ones, so `nova_json_schemas` is listed again when a route has a `json_schema`.

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
- `src/plugins/myapp_body_limit.erl`: refuses request bodies over `max_bytes` (default 65536) before they are read
- `test/myapp_auth_SUITE.erl`

```
$ rebar3 nova gen_auth
```

It then prints the deps (`nova_auth`, `kura`, `kura_postgres`), the `rebar3_kura` hook and the routes to add. The project needs a Kura repo named `myapp_repo`, as `rebar3 nova new --kura` creates. Pin `{nova_auth, "~> 0.2.0"}`: the rate-limit plugin sets nova_auth 0.2's `key_fun` option, which later versions reject.

- Registration takes `email`, `password` and `password_confirmation`. The changesets drop the plaintext from the changeset's changes and params once hashed, valid or not, so it does not reach a log in `{error, Changeset}`.
- `email_changeset/2` (used by `nova_auth_accounts:change_identity/4`) resets `confirmed_at` when the address really changes; `password_changeset/2` hashes the new password.
- The token schema has `family_id` and `used_at`, which `nova_auth_refresh` needs.
- The public route group runs `myapp_rate_limit`, then `myapp_body_limit`, then `nova_request_plugin`, so a flood or an oversized body (or a chunked one, which has no length up front) is refused with 429 or 413 before anything reads it. The bearer group sets `plugins => []`: Nova runs pre-request plugins before security, so a global body-decoding plugin would read the body of a request that has no token at all. The instructions print a protected group for your own routes with `myapp_body_limit` before `nova_request_plugin`, for the same reason. The 413 reply sets no `connection` header, which HTTP/2 forbids.
- `require_bearer/1` reads the `Authorization` header bytewise (anything but a well-formed `Bearer <b64token>` is a 401, never a crash) and puts only the user's `id` and `email` in `auth_data`, never the password hash.
- The rate limit keys on the client address: one bucket per IPv4 address and per IPv6 /64, with IPv4-mapped addresses unmapped. The address is the TCP peer unless the peer is in `{trusted_proxies, [...]}` in the app env (address tuples or `{Address, PrefixLength}` ranges, default `[]`); then it is the right-most `x-forwarded-for` address that is not a trusted proxy. Hops may carry a port (`192.0.2.1:8080`, `[2001:db8::1]:443`) and empty hops are skipped. A mistyped entry, such as a string or a prefix length out of range, raises `invalid_trusted_proxy` on the first request instead of being ignored. Once nova_auth 0.3 is released, its `nova_auth_rate_limit:client_ip/1` replaces this code.
- `login/1` also allows 20 attempts per normalised (NFC, lowercased) email in 15 minutes, whatever the address, so rotating addresses does not buy unlimited guesses. The limit is generous so that failing logins in someone's name hardly locks them out.
- Registration answers 422 for a taken email, which tells anyone which addresses have accounts. Put email confirmation in front of it if that matters.
- The tokens are bearer tokens, so the printed instructions turn Nova's session cookie off with `{use_sessions, false}` in the app's env.

### `nova openapi` — Generate OpenAPI 3.0.3 spec

Generates an OpenAPI 3.0.3 JSON specification from compiled controller routes and any JSON schemas found in `priv/schemas/`. Static files, error pages and inline funs are left out.

```
$ rebar3 nova openapi
===> OpenAPI spec written to /path/to/myapp/priv/assets/openapi.json

$ rebar3 nova openapi --prefix /api --output priv/assets/openapi.json
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

By default the spec goes to `priv/assets`, which the scaffold serves under `/assets`. It lists every route, so pass `--output` with a path outside `priv/assets` to keep it private. No Swagger UI page is written: earlier versions wrote a `swagger.html` that loaded Swagger UI from a CDN. Serve a self-hosted copy if you want one.

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

Checks compiled routes for common security issues and prints findings grouped by severity. The routes are those of the application and of the `nova_apps` declared for it in sys.config (nested ones included), compiled for one Nova environment, so routes that `routes(prod)` returns are audited with `--env prod`. The first line of the report says what was audited, what was not and which config `nova_apps` came from. `nova_apps` are read from the shell config unless `--config` names another; with an `--env` other than `dev` and no `--config`, the report notes that the dev config was used. A `.config.src` works: `${VAR}` and `${VAR:-Default}` are replaced from the environment or the default before it is read, as relx does. The audit sets the Nova environment and the `router_module`s from the config only while it compiles the routes.

`audit` and `openapi` read Nova's route records as nova 0.18.1 defines them. A route whose handler they do not recognise, as a later nova that changes those records would cause, is left out of the report and named in a warning; upgrade rebar3_nova when you see one.

```
$ rebar3 nova audit --env prod --config config/prod_sys.config.src

=== Security Audit ===

  Audited routes(prod) of myapp. Not covered: static files and error pages. nova_apps read from config/prod_sys.config.src.

  WARNINGS:
    POST /users (myapp_users_controller) has no security
    PUT /users/{id} (myapp_users_controller) has no security
    DELETE /users/{id} (myapp_users_controller) has no security

  INFO:
    GET /users (myapp_users_controller) has no security
    GET /health (myapp_health_controller) has no security

  Summary: 3 warning(s), 2 info(s)
```

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--env`, `-e` | no | `dev` | Nova environment passed to the routers' `routes/1` |
| `--config`, `-c` | no | the shell config | sys.config or `.config.src` to read `nova_apps` (and `router_module`) from |
| `--strict`, `-s` | no | `false` | Fail when there are warnings, for CI |

**Rules checked:**
- **Warning**: Mutation methods (POST, PUT, DELETE, PATCH) without a security callback
- **Warning**: Wildcard method (`'_'`) matching all HTTP methods on a route (once per route)
- **Info**: GET routes without a security callback

Static file routes, error pages, websocket and other cowboy handlers, and routes served outside `nova_router` (such as `arizona_nova` routes) are not audited; the coverage line names them. Findings are sorted by path and method.

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
| `security` | `erl_crash.dump` and `.env` presence vs `.gitignore` (asked of `git check-ignore` in a git repository; otherwise a whole `.gitignore` line, so `.envrc` does not count), and a literal `-setcookie` in `config/vm.args(.src)` (a warning, so projects from 0.13.4 and earlier still pass) |
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
