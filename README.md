# Rebar3 plugin for Nova
=====

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

Starts the application with auto-reload. Source file changes are automatically compiled and hot-loaded.

```
$ rebar3 nova serve
```

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

Generated functions return stub responses:
- `list/1`, `show/1`, `update/1` return `{json, #{<<"message">> => <<"TODO">>}}`
- `create/1` returns `{status, 201, #{}, #{<<"message">> => <<"TODO">>}}`
- `delete/1` returns `{status, 204}`

If the file already exists, the command skips it with a warning.

### `nova gen_router` — Generate a router module

Scaffolds a router module implementing the `nova_router` behaviour with an empty routes list.

```
$ rebar3 nova gen_router --name api_v1 --prefix /api/v1
===> Created src/myapp_api_v1_router.erl
```

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--name`, `-n` | yes | — | Router name |
| `--prefix`, `-p` | no | `""` | URL prefix for routes |

### `nova gen_resource` — Generate a full resource

Combines controller generation, JSON schema creation, and prints route snippets to add to your router.

```
$ rebar3 nova gen_resource --name products
===> Created src/controllers/myapp_products_controller.erl
===> Created priv/schemas/product.json
===>
===> Add these routes to your router:
===>   {<<"/products">>, {myapp_products_controller, list}, #{methods => [get]}}
===>   {<<"/products/:id">>, {myapp_products_controller, show}, #{methods => [get]}}
===>   {<<"/products">>, {myapp_products_controller, create}, #{methods => [post]}}
===>   {<<"/products/:id">>, {myapp_products_controller, update}, #{methods => [put]}}
===>   {<<"/products/:id">>, {myapp_products_controller, delete}, #{methods => [delete]}}
```

**Options:** Same as `gen_controller`.

The JSON schema is placed in `priv/schemas/{singular}.json` with `id` and `name` fields as a starting point. The resource name is naively singularized by stripping a trailing "s".

### `nova gen_test` — Generate a Common Test suite

Scaffolds a CT suite with test cases for CRUD actions using `httpc`.

```
$ rebar3 nova gen_test --name users
===> Created test/myapp_users_controller_SUITE.erl
```

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--name`, `-n` | yes | — | Resource name |

The generated suite starts `inets` and the application in `init_per_suite`, then tests each CRUD endpoint against `http://localhost:8080/{name}`.

### `nova openapi` — Generate OpenAPI 3.0.3 spec

Generates an OpenAPI 3.0.3 JSON specification from compiled routes and any JSON schemas found in `priv/schemas/`.

```
$ rebar3 nova openapi
===> OpenAPI spec written to openapi.json
===> Swagger UI written to swagger.html

$ rebar3 nova openapi --output priv/assets/openapi.json --title "My API" --api-version 1.0.0
```

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--output`, `-o` | no | `openapi.json` | Output file path |
| `--title`, `-t` | no | app name | API title |
| `--api-version`, `-v` | no | `0.1.0` | API version string |

A `swagger.html` file is also generated alongside the spec for quick browser-based exploration.

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

Displays all Nova configuration keys read from `sys.config`, showing current values or `(default)` when using built-in defaults.

```
$ rebar3 nova config

=== Nova Configuration ===

  bootstrap_application     myapp
  environment               dev
  cowboy_configuration       #{port => 8080} (default)
  plugins                   [] (default)
  json_lib                  thoas (default)
  use_stacktrace            false (default)
  dispatch_backend          persistent_term (default)
```

Warns if `bootstrap_application` is not set.

### `nova audit` — Audit route security

Checks compiled routes for common security issues and prints findings grouped by severity.

```
$ rebar3 nova audit

=== Security Audit ===

  WARNINGS:
    POST /users (myapp_users_controller) has no security
    PUT /users/1 (myapp_users_controller) has no security
    DELETE /users/1 (myapp_users_controller) has no security

  INFO:
    GET /users (myapp_users_controller) has no security
    GET /health (myapp_health_controller) has no security

  Summary: 3 warning(s), 2 info(s)
```

**Rules checked:**
- **Warning**: Mutation methods (POST, PUT, DELETE, PATCH) without a security callback
- **Warning**: Wildcard method (`'_'`) matching all HTTP methods on a route
- **Info**: GET routes without a security callback

### `nova release` — Build a release

Wraps the standard rebar3 release provider. If `priv/schemas/` exists, regenerates the OpenAPI spec before building.

```
$ rebar3 nova release
===> Regenerating OpenAPI spec...
===> Release built successfully with profile 'prod'

$ rebar3 nova release --profile staging
```

**Options:**

| Flag | Required | Default | Description |
|------|----------|---------|-------------|
| `--profile`, `-p` | no | `prod` | Release profile to use |
