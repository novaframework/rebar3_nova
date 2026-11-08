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
- jesse runs `pattern` with OTP `re` and its default match limit, so a
  pathological pattern costs seconds of CPU per request. `uniqueItems` is
  quadratic in the array length.
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
- Each path segment must match `\A([A-Za-z0-9._~-]+|\{[A-Za-z][A-Za-z0-9_]*\})\z`
  and not be `.` or `..`, which rules out empty segments, segments that bind
  only part of themselves (`{a}.{b}`) and Nova's own `:name` and `[...]`
  syntax. A parameter name follows the identifier rule below, so `{postId}`
  becomes `:post_id`.
- Under the same prefix, every path must bind a position to the same name,
  and no literal segment there may equal that name. `/u/{id}` beside
  `/u/{userId}/posts`, or `/u/{id}` beside `/u/id`, would shadow each other
  in Nova's router, so both are refused.
- Two operations that become the same Nova route, such as `get /pets/{petId}`
  and `get /pets/{pet_id}`, are refused.
- A parameter's `in` must be `path`, `query`, `header` or `cookie`, and
  response keys must be status codes (`200`, `2XX`) or `default`. A method
  key in the wrong case (`GET`), a non-string `operationId` and a non-string
  tag are refused rather than ignored.

### Names

Every name from the spec is validated before any file is written.

- One rule covers every name that becomes an Erlang identifier: tags,
  `operationId`s, security scheme names, path parameter names and names
  derived from a method and path. Each must consist of ASCII letters,
  digits, `_`, `-` and spaces, and is converted from camelCase to
  snake_case and must then match `\A[a-z][a-z0-9_]{0,63}\z` (`\z`, because
  `$` also matches before a trailing newline) and not be an Erlang reserved
  word. A name that becomes a function must also not be an auto-imported
  arity-1 BIF, such as `get` or `length`, or `module_info`. Two names that
  convert to the same identifier, such as the tags `userAdmin` and
  `user_admin`, are refused. Otherwise generation aborts.
- An operation without a tag goes to the `default` controller; one without
  an `operationId` is named from its method and path.
- Component names must match `\A[A-Za-z0-9_-]+(\.[A-Za-z0-9_-]+)*\z`, and two
  that differ only in case are refused, since they would overwrite each other
  on case-insensitive file systems, as are segments that are Windows device
  names (`con`, `aux`, `com1` and so on, in any case). A `.` becomes a
  directory separator, the reverse of how `rebar3 nova openapi` names
  components. The pattern already rules out `..`; the writer still checks
  each path with `filelib:safe_relative_path/2` when it writes the file.
- Summaries and parameter names, which end up in comments, have their line
  breaks replaced before they reach the writer. Strings written into source
  are formatted with `~tp`.

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

- `nullable: true` adds `"null"` to `type` when `type` is set, as OpenAPI
  3.0.3 defines it. It does not change `enum`, so an `enum` without `null`
  still rejects null.
- Boolean `exclusiveMinimum` and `exclusiveMaximum` become the numeric form.
- `readOnly` properties are removed from `required`, or every create request
  would be rejected, and are kept with `not: {}`, so a request that sets one
  fails validation instead of reaching a controller that casts the whole
  body. A readOnly property beside a `$ref` is wrapped as
  `{allOf: [ref], readOnly: true, not: {}}`, so the guard applies there too,
  even though 3.0 ignores `$ref` siblings. `rebar3 nova openapi` drops the
  `not: {}` again, since 3.0's `readOnly` says the same.
- `discriminator`, `example`, `xml`, `externalDocs`, `writeOnly` and `x-*`
  are removed.
- `$id` and `id` abort generation like any keyword outside the list: 3.0
  has neither, and both would change how jesse resolves relative refs.
  `$schema` is set to draft-06.
- `#/components/schemas/X` becomes a path to `X`'s file relative to the file
  holding the ref. Refs to other component types are inlined; a cycle
  through an inlined ref aborts.
- Each `format` jesse does not enforce produces a warning.
- `uniqueItems` must come with a `maxItems` of at most 1024, which bounds
  jesse's quadratic duplicate check.
- A `pattern` must come with a `maxLength` of at most 1024. The pattern is
  refused when it is longer than 200 bytes, uses syntax the scan does not
  model (`\Q...\E`, `(?#...)`, `\c`, `(*` verbs, option groups that turn on
  extended mode), or repeats a group that holds an unbounded or large
  repetition or an alternation (`(a+)+`, `(a|a)*`, `(a+){1,99}`, counting
  PCRE2 forms such as `{1, }` and `{,5}`). It is then run against subjects of
  the longest length `maxLength` allows, each one unit repeated and ending in
  a character that forces a backtrack, with every printable ASCII character
  and each literal run in the pattern as a unit, and refused if any hits a
  match limit of 100,000. `--allow-patterns` lets a refused pattern through
  with a warning.
- The scan and the probe are heuristics, so the generated app also bounds
  every pattern at request time. The generator prints the sys.config entry
  `{jesse, [{re_options, [unicode, ucp, {match_limit, 100000}]}]}`, the same
  limit the probe uses, as it prints the router line. jesse reads its
  `re_options` from the application environment, and a pattern that hits the
  limit returns `nomatch`, so the request is rejected: about 1 ms per value
  against 100 ms or more without it. The limit is node-wide, which is why the
  generator prints it rather than writing it.
- The `maxLength` guard relies on jesse's keyword order: for a schema of up
  to 32 keywords jesse checks them in sorted order and stops at the first
  error, so `maxLength` runs before `pattern`. If nova_json_schemas ever
  collects every error, an oversized string reaches the pattern; the length
  cap and the probe are what bound the cost then, alongside the body-limit
  plugin.

An inline request body becomes `<operation>_request.json`. A request body
that is not `required` gets a warning, because nova_json_schemas validates
whatever body arrives. A body with no `application/json` content, including
`application/*+json` and content types with parameters, gets a warning and
no schema.

### Routes and security

Operations are grouped by their effective security requirement: the
operation's own `security`, or the root `security` when it has none.
Security fails closed.

- Each group lists its own plugins, as `gen_resource` does: `<app>_body_limit`,
  `nova_request_plugin`, then `nova_json_schemas`.
- A route is `{Path, fun Mod:Fun/1, #{methods => [M], extra_state =>
  #{json_schema => "schemas/openapi/..."}}}`.
- Nova takes one security callback per route group, so each distinct
  effective requirement list is one group with one function,
  `security_group_N`, in `<app>_api_security`. Lists that differ only in the
  order of their alternatives, schemes or scopes are the same group. The
  function returns `{false, 401, #{}}` until it is written, whatever the list
  holds, including an empty `{}` alternative and OAuth2 scopes. Nothing
  claims scopes are enforced. Group names that clash with a security scheme's
  function are refused.
- An operation with no requirement, from neither the operation nor the root,
  gets the group `security_undeclared`, the same 401 stub. Only an explicit
  `security: []` gives a group with `security => false`.
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
  `rebar3 nova openapi` run on the compiled app, keeping only the routes
  served by the generated controllers listed in the manifest, so a health
  check or other hand-written route in the same app is not a difference.
  It writes nothing and exits non-zero on a difference. An operation it
  cannot read, such as one without an `operationId`, is reported as a
  difference.
- `rebar3 nova openapi` emits OpenAPI 3.0 schemas (`nullable`, boolean
  exclusive bounds), so its schemas are translated back to draft-06 before
  they are compared.
- Round trip: the two projections are equal for a freshly generated app.
- Fixed point: the output of `rebar3 nova openapi` on a generated app is a
  3.0 spec the generator accepts, and generating from it gives the same
  projection. Files are not compared byte for byte, because the forward
  spec drops what is outside the projection.

Responses, parameter types, query and header parameters, security, `info`
and descriptions are outside the projection, because `rebar3 nova openapi`
synthesises or drops them. In particular `--check` does not notice a
security function that was edited to let everyone in.

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
- The generator depends on nova_json_schemas ADR 0001, on
  `rebar3 nova openapi` pointing component `$ref`s at components, and on it
  emitting OpenAPI 3.0 schema keywords.
- The keyword allow-list follows jesse. If nova_json_schemas moves to a
  validator with JSON Schema 2020-12, 3.1 support should be revisited.
