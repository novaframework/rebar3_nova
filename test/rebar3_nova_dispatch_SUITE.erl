%%% Covers reading a compiled Nova dispatch table.
%%%
%%% The route-reporting tasks used to walk routing_tree's records directly.
%%% They now go through rebar3_nova_dispatch, and this suite builds a real
%%% dispatch table with nova_router and checks each task can still read it.
-module(rebar3_nova_dispatch_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
    routes_are_flattened/1,
    status_code_routes_are_left_out/1,
    openapi_paths_use_braces/1,
    catch_all_is_dropped_from_openapi_paths/1,
    methods_are_lowercased/1,
    audit_reads_the_table/1,
    openapi_reads_the_table/1,
    doctor_reads_the_table/1,
    routes_task_prints_the_table/1,
    audit_reports_a_wildcard_once/1,
    audit_findings_are_sorted/1,
    openapi_responses_follow_the_method/1,
    openapi_schema_is_the_request_body_only/1,
    openapi_component_schemas_drop_json_schema_keywords/1,
    openapi_prefix_limits_the_paths/1,
    openapi_operation_ids_are_unique/1,
    openapi_nested_schemas_are_loaded_by_relative_path/1,
    openapi_same_basename_schemas_do_not_collide/1,
    openapi_component_name_collision_is_an_error/1,
    openapi_unknown_schema_gets_no_ref/1,
    openapi_default_output_is_the_assets_dir/1,
    unrecognised_handlers_are_reported/1,
    audit_compiles_the_requested_environment/1,
    audit_covers_the_nova_apps_in_the_config/1,
    audit_strict_fails_on_a_warning/1,
    audit_reads_a_prod_config_src/1,
    audit_names_the_config_and_warns_without_one/1,
    audit_restores_the_environment/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        routes_are_flattened,
        status_code_routes_are_left_out,
        openapi_paths_use_braces,
        catch_all_is_dropped_from_openapi_paths,
        methods_are_lowercased,
        audit_reads_the_table,
        openapi_reads_the_table,
        doctor_reads_the_table,
        routes_task_prints_the_table,
        audit_reports_a_wildcard_once,
        audit_findings_are_sorted,
        openapi_responses_follow_the_method,
        openapi_schema_is_the_request_body_only,
        openapi_component_schemas_drop_json_schema_keywords,
        openapi_prefix_limits_the_paths,
        openapi_operation_ids_are_unique,
        openapi_nested_schemas_are_loaded_by_relative_path,
        openapi_same_basename_schemas_do_not_collide,
        openapi_component_name_collision_is_an_error,
        openapi_unknown_schema_gets_no_ref,
        openapi_default_output_is_the_assets_dir,
        unrecognised_handlers_are_reported,
        audit_compiles_the_requested_environment,
        audit_covers_the_nova_apps_in_the_config,
        audit_strict_fails_on_a_warning,
        audit_reads_a_prod_config_src,
        audit_names_the_config_and_warns_without_one,
        audit_restores_the_environment
    ].

init_per_suite(Config) ->
    _ = application:load(nova),
    _ = application:load(rebar3_nova),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(rebar3_nova, router_module, rebar3_nova_test_router),
    rebar3_nova_test_router:set_routes(routes()),
    _ = persistent_term:erase(nova_dispatch),
    _ = persistent_term:erase(nova_apps),
    [{dispatch, nova_router:compile([rebar3_nova])} | Config].

end_per_suite(_Config) ->
    application:unset_env(rebar3_nova, router_module),
    _ = persistent_term:erase(nova_dispatch),
    _ = persistent_term:erase(nova_apps),
    ok.

%%====================================================================
%% A real Nova 0.18 dispatch table covering the shapes the tasks care about
%%====================================================================

routes() ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                {"/users", fun test_controller:index/1, #{methods => [get, post]}},
                {"/users/:id", fun test_controller:index/1, #{methods => [get]}},
                {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                {"/assets/[...]", "templates"},
                {"/robots.txt", "templates/nova_plugin.template"},
                {"/ws", test_ws, #{protocol => ws}},
                {404, fun nova_error_controller:not_found/1, #{}}
            ]
        }
    ].

%%====================================================================
%% rebar3_nova_dispatch
%%====================================================================

routes_are_flattened(Config) ->
    Routes = rebar3_nova_dispatch:routes(?config(dispatch, Config)),
    Paths = lists:usort([Path || {Path, _Method, _Payload} <- Routes]),
    ?assertEqual(
        [
            <<"/assets/[...]">>,
            <<"/heartbeat">>,
            <<"/robots.txt">>,
            <<"/users">>,
            <<"/users/:id">>,
            <<"/ws">>
        ],
        Paths
    ),
    %% /users carries two methods, so it appears twice.
    ?assertEqual(2, length([P || {P, _M, _V} <- Routes, P =:= <<"/users">>])).

status_code_routes_are_left_out(Config) ->
    Routes = rebar3_nova_dispatch:routes(?config(dispatch, Config)),
    ?assertEqual([], [R || R = {Path, _M, _V} <- Routes, not is_binary(Path)]).

openapi_paths_use_braces(_Config) ->
    ?assertEqual(<<"/users/{id}">>, rebar3_nova_dispatch:openapi_path(<<"/users/:id">>)),
    ?assertEqual(<<"/users">>, rebar3_nova_dispatch:openapi_path(<<"/users">>)),
    ?assertEqual(<<"/a/{b}/c/{d}">>, rebar3_nova_dispatch:openapi_path(<<"/a/:b/c/:d">>)),
    ?assertEqual(<<"/">>, rebar3_nova_dispatch:openapi_path(<<"/">>)).

catch_all_is_dropped_from_openapi_paths(_Config) ->
    ?assertEqual(<<"/assets">>, rebar3_nova_dispatch:openapi_path(<<"/assets/[...]">>)),
    ?assertEqual(<<"/">>, rebar3_nova_dispatch:openapi_path(<<"/[...]">>)).

methods_are_lowercased(_Config) ->
    ?assertEqual(<<"get">>, rebar3_nova_dispatch:method_to_binary(<<"GET">>)),
    ?assertEqual(<<"any">>, rebar3_nova_dispatch:method_to_binary('_')),
    ?assertEqual(<<"post">>, rebar3_nova_dispatch:method_to_binary(post)).

%%====================================================================
%% The tasks
%%====================================================================

%% The audit skips static files, error pages and websocket handlers. Nova
%% 0.18 stores static routes as nova_file_controller funs with
%% `module = undefined`, so they must be recognised from the callback.
audit_reads_the_table(Config) ->
    Routes = rebar3_nova_audit:collect_routes(?config(dispatch, Config)),
    Paths = lists:usort([Path || {Path, _M, _S, _Mod, _Any} <- Routes]),
    ?assertEqual([<<"/heartbeat">>, <<"/users">>, <<"/users/{id}">>], Paths),
    ?assertEqual(
        [<<"get">>, <<"post">>],
        lists:usort([M || {<<"/users">>, M, _S, _Mod, _Any} <- Routes])
    ).

%% Static files, error pages and inline funs have no place in an API spec.
openapi_reads_the_table(Config) ->
    Routes = rebar3_nova_openapi:collect_routes(?config(dispatch, Config)),
    Paths = lists:usort([Path || {Path, _M, _Mod, _F, _E} <- Routes]),
    ?assertEqual([<<"/users">>, <<"/users/{id}">>], Paths),
    ?assert(lists:all(fun({_P, _M, Mod, _F, _E}) -> Mod =:= test_controller end, Routes)).

%% The doctor reports on every route, websockets included.
doctor_reads_the_table(Config) ->
    Handlers = rebar3_nova_doctor:collect_route_handlers(?config(dispatch, Config)),
    ?assertEqual(7, length(Handlers)),
    ?assertMatch(
        [{<<"/users/:id">>, <<"GET">>, test_controller, index, 1}],
        [H || H = {<<"/users/:id">>, _M, _Mod, _F, _A} <- Handlers]
    ),
    ?assertMatch(
        [{<<"/ws">>, '_', nova_ws_handler, init, 2}],
        [H || H = {<<"/ws">>, _M, _Mod, _F, _A} <- Handlers]
    ),
    ?assert(lists:all(fun({_P, _M, Mod, _F, _A}) -> Mod =/= unknown end, Handlers)).

routes_task_prints_the_table(Config) ->
    Output = capture(fun() -> rebar3_nova_routes:print_routes(?config(dispatch, Config)) end),
    ?assertNotEqual(nomatch, string:find(Output, "/users/:id ")),
    ?assertNotEqual(nomatch, string:find(Output, "test_controller:index/1")),
    ?assertNotEqual(nomatch, string:find(Output, "(status 404)")),
    ?assertNotEqual(nomatch, string:find(Output, "nova_ws_handler")).

audit_reports_a_wildcard_once(_Config) ->
    Routes = [
        {~"/any", M, false, test_controller, true}
     || M <- [~"get", ~"post", ~"put", ~"delete", ~"patch"]
    ],
    {Warnings, _Infos} = rebar3_nova_audit:classify_findings(Routes),
    Wildcards = [W || W <- Warnings, string:find(W, "Wildcard") =/= nomatch],
    ?assertEqual(1, length(Wildcards)).

audit_findings_are_sorted(_Config) ->
    Routes = [
        {~"/b", ~"post", false, test_controller, false},
        {~"/a", ~"delete", false, test_controller, false},
        {~"/a", ~"get", false, test_controller, false},
        {~"/c", ~"get", false, test_controller, false},
        {~"/a", ~"post", false, test_controller, false}
    ],
    Expected = rebar3_nova_audit:classify_findings(lists:sort(Routes)),
    ?assertEqual(Expected, rebar3_nova_audit:classify_findings(lists:reverse(Routes))),
    {Warnings, Infos} = Expected,
    ?assertEqual(
        [
            "DELETE /a (test_controller) has no security",
            "POST /a (test_controller) has no security",
            "POST /b (test_controller) has no security"
        ],
        [lists:flatten(W) || W <- Warnings]
    ),
    ?assertEqual(
        ["GET /a (test_controller) has no security", "GET /c (test_controller) has no security"],
        [lists:flatten(I) || I <- Infos]
    ).

openapi_responses_follow_the_method(_Config) ->
    Paths = spec_paths([
        {~"/tags", ~"get", test_controller, list, undefined},
        {~"/tags", ~"post", test_controller, create, undefined},
        {~"/tags/{id}", ~"delete", test_controller, delete, undefined},
        {~"/tags/{id}", ~"put", test_controller, update, undefined}
    ]),
    ?assertEqual([~"200"], response_codes(Paths, ~"/tags", ~"get")),
    ?assertEqual([~"201"], response_codes(Paths, ~"/tags", ~"post")),
    ?assertEqual([~"204", ~"404"], response_codes(Paths, ~"/tags/{id}", ~"delete")),
    ?assertEqual([~"200", ~"404"], response_codes(Paths, ~"/tags/{id}", ~"put")).

openapi_schema_is_the_request_body_only(_Config) ->
    Extra = #{json_schema => "schemas/tag.json"},
    Paths = spec_paths(
        [
            {~"/tags", ~"get", test_controller, list, Extra},
            {~"/tags", ~"post", test_controller, create, Extra}
        ],
        #{~"tag" => #{~"type" => ~"object"}}
    ),
    #{~"get" := Get, ~"post" := Post} = maps:get(~"/tags", Paths),
    ?assertNot(maps:is_key(~"requestBody", Get)),
    ?assertEqual([~"200"], response_codes(Paths, ~"/tags", ~"get")),
    [?assertNot(maps:is_key(~"content", R)) || R <- maps:values(maps:get(~"responses", Get))],
    ?assertMatch(
        #{~"content" := #{~"application/json" := #{~"schema" := #{~"$ref" := _}}}},
        maps:get(~"requestBody", Post)
    ),
    ?assertEqual([~"201", ~"400"], response_codes(Paths, ~"/tags", ~"post")),
    [?assertNot(maps:is_key(~"content", R)) || R <- maps:values(maps:get(~"responses", Post))].

openapi_component_schemas_drop_json_schema_keywords(_Config) ->
    Schema = #{
        ~"$schema" => ~"http://json-schema.org/draft-06/schema#",
        ~"$id" => ~"tag",
        ~"type" => ~"object",
        ~"properties" => #{~"id" => #{~"type" => ~"integer"}}
    },
    ?assertEqual(
        #{~"type" => ~"object", ~"properties" => #{~"id" => #{~"type" => ~"integer"}}},
        rebar3_nova_openapi:component_schema(Schema)
    ),
    ?assertEqual(
        #{~"type" => ~"object"},
        rebar3_nova_openapi:component_schema(#{~"id" => ~"tag", ~"type" => ~"object"})
    ).

openapi_prefix_limits_the_paths(Config) ->
    Dispatch = ?config(dispatch, Config),
    Users = rebar3_nova_openapi:collect_routes(Dispatch, ~"/users"),
    ?assertEqual(
        [~"/users", ~"/users/{id}"], lists:usort([P || {P, _, _, _, _} <- Users])
    ),
    ?assertEqual([], rebar3_nova_openapi:collect_routes(Dispatch, ~"/use")),
    ?assertEqual([], rebar3_nova_openapi:collect_routes(Dispatch, ~"/api")).

openapi_operation_ids_are_unique(_Config) ->
    Paths = spec_paths([
        {~"/a", ~"get", test_controller, index, undefined},
        {~"/b", ~"get", test_controller, index, undefined},
        {~"/c", ~"get", test_controller, show, undefined}
    ]),
    Ids = [
        maps:get(~"operationId", Op)
     || Methods <- maps:values(Paths), Op <- maps:values(Methods)
    ],
    ?assertEqual(length(Ids), length(lists:usort(Ids))),
    ?assert(lists:member(~"test_controller.show", Ids)).

%% nova_json_schemas keys schemas by their path under priv/schemas, so a
%% route's "schemas/nested/x.json" must resolve to a loaded component.
openapi_nested_schemas_are_loaded_by_relative_path(Config) ->
    AppDir = filename:join(?config(priv_dir, Config), "nested_app"),
    SchemaDir = filename:join([AppDir, "priv", "schemas"]),
    write_schema(SchemaDir, "tag.json", #{~"type" => ~"object"}),
    write_schema(SchemaDir, "nested/x.json", #{
        ~"$schema" => ~"http://json-schema.org/draft-06/schema#", ~"type" => ~"object"
    }),
    Schemas = rebar3_nova_openapi:load_schemas(AppDir),
    ?assertEqual([~"nested.x", ~"tag"], lists:sort(maps:keys(Schemas))),
    ?assertEqual(#{~"type" => ~"object"}, maps:get(~"nested.x", Schemas)),
    Paths = spec_paths(
        [{~"/x", ~"post", test_controller, create, #{json_schema => "schemas/nested/x.json"}}],
        Schemas
    ),
    ?assertEqual(~"#/components/schemas/nested.x", request_ref(Paths, ~"/x", ~"post")).

openapi_same_basename_schemas_do_not_collide(Config) ->
    AppDir = filename:join(?config(priv_dir, Config), "versioned_app"),
    SchemaDir = filename:join([AppDir, "priv", "schemas"]),
    write_schema(SchemaDir, "v1/user.json", #{~"title" => ~"v1"}),
    write_schema(SchemaDir, "v2/user.json", #{~"title" => ~"v2"}),
    Schemas = rebar3_nova_openapi:load_schemas(AppDir),
    ?assertEqual(
        #{~"v1.user" => #{~"title" => ~"v1"}, ~"v2.user" => #{~"title" => ~"v2"}}, Schemas
    ),
    Paths = spec_paths(
        [
            {~"/v1/users", ~"post", test_controller, create, #{
                json_schema => "schemas/v1/user.json"
            }},
            {~"/v2/users", ~"post", test_controller, create, #{
                json_schema => "schemas/v2/user.json"
            }}
        ],
        Schemas
    ),
    ?assertEqual(~"#/components/schemas/v1.user", request_ref(Paths, ~"/v1/users", ~"post")),
    ?assertEqual(~"#/components/schemas/v2.user", request_ref(Paths, ~"/v2/users", ~"post")).

openapi_component_name_collision_is_an_error(_Config) ->
    ?assertMatch(
        {error, {collision, ~"a.b", ["a.b.json", "a/b.json"]}},
        rebar3_nova_openapi:component_names(["a/b.json", "a.b.json"])
    ),
    ?assertMatch({ok, [_, _]}, rebar3_nova_openapi:component_names(["a/b.json", "a_b.json"])).

openapi_unknown_schema_gets_no_ref(_Config) ->
    Paths = spec_paths([
        {~"/x", ~"post", test_controller, create, #{json_schema => "schemas/missing.json"}}
    ]),
    ?assertNot(maps:is_key(~"requestBody", maps:get(~"post", maps:get(~"/x", Paths)))).

%%====================================================================
%% Helpers
%%====================================================================

spec_paths(Routes) ->
    spec_paths(Routes, #{}).

spec_paths(Routes, Schemas) ->
    maps:get(~"paths", rebar3_nova_openapi:build_spec("t", "1", Routes, Schemas)).

write_schema(Dir, RelPath, Schema) ->
    Path = filename:join(Dir, RelPath),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, json:encode(Schema)).

request_ref(Paths, Path, Method) ->
    #{
        ~"requestBody" := #{
            ~"content" := #{~"application/json" := #{~"schema" := #{~"$ref" := Ref}}}
        }
    } =
        maps:get(Method, maps:get(Path, Paths)),
    Ref.

response_codes(Paths, Path, Method) ->
    lists:sort(maps:keys(maps:get(~"responses", maps:get(Method, maps:get(Path, Paths))))).

capture(Fun) ->
    Self = self(),
    Ref = make_ref(),
    {Pid, MonRef} =
        spawn_monitor(fun() ->
            group_leader(spawn_capture(Self, Ref), self()),
            Fun(),
            exit(normal)
        end),
    receive
        {'DOWN', MonRef, process, Pid, _Reason} -> ok
    after 5000 ->
        ct:fail(capture_timeout)
    end,
    collect_output(Ref, []).

spawn_capture(Owner, Ref) ->
    spawn(fun() -> capture_loop(Owner, Ref) end).

capture_loop(Owner, Ref) ->
    receive
        {io_request, From, ReplyAs, {put_chars, unicode, Chars}} ->
            Owner ! {Ref, unicode:characters_to_list(Chars)},
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Owner, Ref);
        {io_request, From, ReplyAs, {put_chars, unicode, M, F, A}} ->
            Owner ! {Ref, unicode:characters_to_list(apply(M, F, A))},
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Owner, Ref);
        {io_request, From, ReplyAs, _Other} ->
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Owner, Ref)
    end.

collect_output(Ref, Acc) ->
    receive
        {Ref, Chars} -> collect_output(Ref, [Chars | Acc])
    after 0 ->
        lists:flatten(lists:reverse(Acc))
    end.

%% As before 0.14: the scaffold serves priv/assets under /assets.
openapi_default_output_is_the_assets_dir(_Config) ->
    ?assertEqual("/app/priv/assets/openapi.json", rebar3_nova_openapi:default_output("/app")).

%% A nova that reshapes its handler records makes routes unknown, and the
%% tasks would drop them without a word.
unrecognised_handlers_are_reported(Config) ->
    ?assertEqual(
        [],
        rebar3_nova_dispatch:unknown_routes(rebar3_nova_dispatch:routes(?config(dispatch, Config)))
    ),
    Future = {nova_handler_value, test_app, a_later_shape},
    {ok, Trie} = nova_routing_trie:insert(~"/future", ~"GET", Future, nova_routing_trie:new()),
    Routes = rebar3_nova_dispatch:routes(Trie),
    ?assertEqual([{~"/future", ~"GET"}], rebar3_nova_dispatch:unknown_routes(Routes)),
    ?assertEqual([], rebar3_nova_openapi:collect_routes(Trie)),
    ?assertEqual([], rebar3_nova_audit:collect_routes(Trie)).

%% A route only routes(prod) returns used to be invisible: the audit always
%% compiled the dev routes.
audit_compiles_the_requested_environment(_Config) ->
    with_prod_only_route(fun() ->
        #{warnings := Prod, coverage := Coverage} = rebar3_nova_audit:audit(rebar3_nova, prod, []),
        #{warnings := Dev} = rebar3_nova_audit:audit(rebar3_nova, dev, []),
        ?assertEqual(1, length(mentioning("/admin", Prod))),
        ?assertEqual([], mentioning("/admin", Dev)),
        ?assertNotEqual(nomatch, string:find(Coverage, "routes(prod) of rebar3_nova"))
    end).

audit_covers_the_nova_apps_in_the_config(_Config) ->
    Config = [
        {rebar3_nova, [{nova_apps, [{rebar3_nova_audit_sub, #{prefix => "/sub"}}]}]},
        {rebar3_nova_audit_sub, [{router_module, rebar3_nova_test_router}]},
        {arizona_nova, [{routes, []}]}
    ],
    with_prod_only_route(fun() ->
        try
            #{warnings := Warnings, coverage := Coverage} =
                rebar3_nova_audit:audit(rebar3_nova, prod, Config),
            ?assertEqual(1, length(mentioning("/sub/admin", Warnings))),
            ?assertNotEqual(
                nomatch, string:find(Coverage, "of rebar3_nova, rebar3_nova_audit_sub")
            ),
            ?assertNotEqual(nomatch, string:find(Coverage, "arizona_nova routes"))
        after
            application:unset_env(rebar3_nova_audit_sub, router_module)
        end
    end).

audit_strict_fails_on_a_warning(_Config) ->
    with_prod_only_route(fun() ->
        Options = #{env => prod, config => []},
        ?assertEqual(ok, rebar3_nova_audit:run(rebar3_nova, Options#{strict => false})),
        ?assertMatch({error, _}, rebar3_nova_audit:run(rebar3_nova, Options#{strict => true})),
        ?assertEqual(
            ok, rebar3_nova_audit:run(rebar3_nova, #{env => dev, config => [], strict => true})
        )
    end).

%% Unquoted ${VAR:-Default} values, as `rebar3 nova new` writes them for
%% prod, are not Erlang terms until they are replaced.
audit_reads_a_prod_config_src(Config) ->
    Path = filename:join(?config(priv_dir, Config), "prod_sys.config.src"),
    ok = file:write_file(
        Path,
        ~"""
        %% -*- mode: erlang -*-
        [
         {kura, [
             {ssl, ${DATABASE_SSL:-true}},
             {repos, #{blog_repo => #{
                 host => "${DATABASE_HOST}",
                 port => ${DATABASE_PORT:-5432},
                 database => "${DATABASE_NAME:-blog}",
                 pool_size => ${DATABASE_POOL_SIZE:-10}
             }}}
         ]},
         {rebar3_nova, [{nova_apps, [{rebar3_nova_audit_sub, #{prefix => "/sub"}}]}]},
         {rebar3_nova_audit_sub, [{router_module, rebar3_nova_test_router}]}
        ].
        """
    ),
    Parsed = rebar3_nova_audit:consult_config(Path, true),
    ?assertMatch(
        [{kura, [{ssl, true}, {repos, #{blog_repo := #{port := 5432, database := "blog"}}}]} | _],
        Parsed
    ),
    with_prod_only_route(fun() ->
        try
            #{warnings := Warnings} = rebar3_nova_audit:audit(rebar3_nova, prod, Parsed),
            ?assertEqual(1, length(mentioning("/sub/admin", Warnings)))
        after
            application:unset_env(rebar3_nova_audit_sub, router_module)
        end
    end).

audit_names_the_config_and_warns_without_one(_Config) ->
    Base = #{env => prod, strict => false, config => [], config_file => "config/dev_sys.config"},
    Shell = printed(fun() -> rebar3_nova_audit:run(rebar3_nova, Base#{explicit_config => false}) end),
    ?assertNotEqual(nomatch, string:find(Shell, "nova_apps read from config/dev_sys.config.")),
    ?assertNotEqual(nomatch, string:find(Shell, "pass --config")),
    Given = printed(fun() ->
        rebar3_nova_audit:run(rebar3_nova, Base#{
            config_file => "config/prod_sys.config.src", explicit_config => true
        })
    end),
    ?assertNotEqual(nomatch, string:find(Given, "nova_apps read from config/prod_sys.config.src.")),
    ?assertEqual(nomatch, string:find(Given, "pass --config")),
    Dev = printed(fun() ->
        rebar3_nova_audit:run(rebar3_nova, Base#{env => dev, explicit_config => false})
    end),
    ?assertEqual(nomatch, string:find(Dev, "pass --config")).

audit_restores_the_environment(_Config) ->
    Config = [{rebar3_nova_audit_sub, [{router_module, rebar3_nova_test_router}]}],
    application:unset_env(nova, environment),
    application:unset_env(rebar3_nova_audit_sub, router_module),
    _ = rebar3_nova_audit:audit(rebar3_nova, prod, Config),
    ?assertEqual(undefined, application:get_env(nova, environment)),
    ?assertEqual(undefined, application:get_env(rebar3_nova_audit_sub, router_module)),
    application:set_env(nova, environment, test),
    application:set_env(rebar3_nova_audit_sub, router_module, other_router),
    try
        _ = rebar3_nova_audit:audit(rebar3_nova, prod, Config),
        ?assertEqual({ok, test}, application:get_env(nova, environment)),
        ?assertEqual({ok, other_router}, application:get_env(rebar3_nova_audit_sub, router_module))
    after
        application:unset_env(nova, environment),
        application:unset_env(rebar3_nova_audit_sub, router_module)
    end.

printed(Fun) ->
    ct:capture_start(),
    try
        ok = Fun()
    after
        ct:capture_stop()
    end,
    lists:flatten(ct:capture_get()).

with_prod_only_route(Fun) ->
    rebar3_nova_test_router:set_routes(fun
        (prod) -> [#{routes => [{"/admin", fun test_controller:admin/1, #{methods => [post]}}]}];
        (_) -> []
    end),
    try
        Fun()
    after
        rebar3_nova_test_router:set_routes(routes())
    end.

mentioning(Text, Lines) ->
    [Line || Line <- Lines, string:find(Line, Text) =/= nomatch].
