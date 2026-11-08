# 1. Generate Nova routes and controllers from an OpenAPI spec

Date: 2026-11-08

## Status

Proposed

## Context

`rebar3 nova openapi` writes a spec from an app's routes. Teams that write
the spec first need the reverse: a Nova app generated from the spec.

The existing generators do not fit. OpenAPI Generator's `erlang-server`
target, and rebar3_openapi which wraps it, emit plain Cowboy handlers and
need Java. Neither emits Nova route groups or controllers.

What the generated app can enforce is set by what Nova validates today:

- nova_json_schemas validates request bodies only, from
  `extra_state => #{json_schema => Path}` (nova_json_schemas ADR 0001).
  A `json_schema` key outside `extra_state` is ignored.
- It uses jesse 1.8.1, which supports draft-03, draft-04 and draft-06.
  jesse ignores keywords it does not know, so an untranslated `nullable` or
  `unevaluatedProperties` silently stops constraining anything. It resolves
  a `$ref` against the file that holds it, and its default loader fetches
  `file://` and `http(s)://` refs while serving a request.
- Path parameters, query parameters and responses are not validated.
- A route group's `security` defaults to open, and pre-request plugins run
  before the security callback, so schema validation runs for callers with
  no credentials.
- jesse runs `pattern` and `patternProperties` with OTP `re` and its default
  match limit, so a pathological pattern costs seconds of CPU per request.
  `uniqueItems` is quadratic in the array length.
- Nova reads `:name` in a route path as a binding and `[...]` as a
  catch-all.

The spec may come from a third party, and its names end up in Erlang source
under `src/` that is compiled on developer machines and in CI.

## Decision

The provider is `rebar3 nova gen_from_openapi --spec FILE [--check]`. If the
unified `rebar3 nova gen` command lands first, it becomes a kind of that
command with the same behaviour.

### Input

- JSON only. YAML specs are converted first (`yq -o json`); the plugin takes
  no YAML dependency, because a rebar3 plugin's deps load into every user's
  rebar3.
- OpenAPI 3.0.x only. A 3.1 spec is refused.
- The main application only, the one nova_json_schemas loads schemas from.
- Refused: remote and `file://` refs, and request bodies on `GET`, `HEAD`
  and `DELETE`, which `nova_request_plugin` does not decode.
- Each path segment must match `\A([A-Za-z0-9._~-]+|\{[a-z][a-z0-9_]*\})\z`
  and not be `.` or `..`, which rules out empty segments, segments that bind
  only part of themselves (`{a}.{b}`) and Nova's own `:name` and `[...]`
  syntax. `{p}` then becomes `:p`.

### Names

Every name from the spec is validated before any file is written.

- One rule covers every name that becomes an Erlang identifier: tags,
  `operationId`s, security scheme names, path parameter names and names
  derived from a method and path. Each is converted from camelCase to
  snake_case and must then match `\A[a-z][a-z0-9_]{0,63}\z` (`\z`, because
  `$` also matches before a trailing newline) and not be an Erlang reserved
  word. Two names that convert to the same identifier are refused.
  Otherwise generation aborts.
- An operation without a tag goes to the `default` controller; one without
  an `operationId` is named from its method and path.
- Component names must match `\A[A-Za-z0-9_-]+(\.[A-Za-z0-9_-]+)*\z`, and two
  that differ only in case are refused, since they would overwrite each other
  on case-insensitive file systems. A `.` becomes a directory separator, the
  reverse of how `rebar3 nova openapi` names components, and the resulting
  path is checked with `filelib:safe_relative_path/2`.
- Text copied into comments has its newlines removed. Strings written into
  source are formatted with `~tp`.

### Ownership

| Output | Path | On a later run |
| --- | --- | --- |
| Schemas | `priv/schemas/openapi/` | Regenerated. Files no longer in the spec are deleted, using a manifest of what was written. |
| Routes | `src/<app>_api_routes.erl` | Written once |
| Controllers | `src/controllers/<app>_<tag>_controller.erl` | Written once |
| Security | `src/<app>_api_security.erl` | Written once |
| Test suites | `test/`, through `gen_test` | Written once |

The manifest is input like the spec: each entry is checked with
`filelib:safe_relative_path/2` against `priv/schemas/openapi` and must be a
regular file before it is deleted. The generator refuses to write when
`priv/schemas/openapi` is a symlink.

The generator never edits the app's router. It prints the line that appends
`<app>_api_routes:routes(Env)` to it.

### Schemas

3.0 Schema Objects are translated to draft-06 from an allow-list of
keywords. A keyword outside the list aborts generation, because jesse would
ignore it.

- `nullable: true` adds `"null"` to `type`, and to `enum` when there is one.
- Boolean `exclusiveMinimum` and `exclusiveMaximum` become the numeric form.
- `readOnly` properties are removed from `required`, or every create request
  would be rejected.
- `discriminator`, `example`, `xml`, `externalDocs`, `writeOnly` and `x-*`
  are removed.
- `$id` and `id` are removed, since they change how jesse resolves relative
  refs. `$schema` is set to draft-06.
- `#/components/schemas/X` becomes a path to `X`'s file relative to the file
  holding the ref. Refs to other component types are inlined; a cycle
  through an inlined ref aborts.
- Each `format` jesse does not enforce produces a warning.
- A `pattern` must come with `maxLength`, and `uniqueItems` with `maxItems`.
  Each `pattern` and `patternProperties` key is linted: it is refused when it
  is longer than a fixed cap or nests quantifiers. `--allow-patterns` lets a
  refused pattern through with a warning.

An inline request body becomes `<operation>_request.json`.

### Routes and security

Operations are grouped by their effective security requirement: the
operation's own `security`, or the root `security` when it has none.
Security fails closed.

- Each group lists its own plugins, as `gen_resource` does: `<app>_body_limit`,
  `nova_request_plugin`, then `nova_json_schemas`.
- A route is `{Path, fun Mod:Fun/1, #{methods => [M], extra_state =>
  #{json_schema => "schemas/openapi/..."}}}`.
- Each requirement, including an empty `{}` alternative and OAuth2 scopes,
  gets a function in `<app>_api_security` that returns `{false, 401, #{}}`
  until it is written. Nothing claims scopes are enforced.
- An operation with no requirement, from neither the operation nor the root,
  gets the same 401 stub. Only an explicit `security: []` gives a group with
  `security => false`.
- A requirement that names an undefined scheme aborts generation.
- Groups with security leave `render_errors` off, because schema errors are
  produced before the security callback runs.

### Open question

Because validation runs before the security callback, a caller with no
credentials gets `400` for an invalid body and `401` for a valid one, and can
map the schema that way even with `render_errors` off. One fix is a
generated `<app>_api_auth` pre-request plugin, listed after
`<app>_body_limit` and before `nova_request_plugin`, so unauthenticated
requests stop before the body is decoded. That moves authentication out of
Nova's `security` callback, which needs a ruling before this ADR is
accepted.

### Controllers

Each operation becomes a function that replies `501`. Comments list its path
and query parameters and its declared responses, none of which Nova
validates. Route keys that no plugin reads are not generated.

### `--check` and round-trip equivalence

One projection is used by both `--check` and the tests. It maps each path
and method to its path parameter names, its translated request schema and
its controller and function.

- `--check` compares the projection of the spec with the projection of
  `rebar3 nova openapi` run on the compiled app. It writes nothing and exits
  non-zero on a difference.
- Round trip: the two projections are equal for a freshly generated app.
- Fixed point: generating from the output of `rebar3 nova openapi` writes
  identical files.

Responses, parameter types, query and header parameters, security, `info`
and descriptions are outside the projection, because `rebar3 nova openapi`
synthesises or drops them.

## Consequences

- Nothing changes in nova. In nova_json_schemas the generator depends on
  the `schema_loader_fun` that limits ref loading to schemas loaded from
  `priv/schemas` (nova_json_schemas#25); without it, a ref that misses at
  request time goes through jesse's default loader. The generator never
  emits `jesse_options`, which would replace that loader.
- Spec features Nova cannot enforce appear as TODO comments, not as route
  keys that look enforced.
- A generated app rejects requests to secured operations until their
  security functions are written.
- 3.1 and YAML specs need converting before generation.
- The generator depends on nova_json_schemas ADR 0001 and on
  `rebar3 nova openapi` pointing component `$ref`s at components.
- The keyword allow-list follows jesse. If nova_json_schemas moves to a
  validator with JSON Schema 2020-12, 3.1 support should be revisited.
