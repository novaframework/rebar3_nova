-module(rebar3_nova_gen_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    controller_actions_take_a_nova_request/1,
    controller_is_erlfmt_formatted/1,
    resource_schema_is_draft_06/1,
    resource_schema_does_not_require_id/1,
    resource_route_hints_use_funs/1,
    resource_write_hints_are_behind_security/1,
    resource_write_hints_limit_the_body/1,
    resource_write_hints_name_their_placeholders/1,
    test_suite_follows_the_routes/1,
    test_suite_compiles/1,
    test_suite_is_erlfmt_formatted/1,
    test_suite_reads_the_port_at_runtime/1
]).

-define(ACTIONS, [list, show, create, update, delete]).

all() ->
    [
        controller_actions_take_a_nova_request,
        controller_is_erlfmt_formatted,
        resource_schema_is_draft_06,
        resource_schema_does_not_require_id,
        resource_route_hints_use_funs,
        resource_write_hints_are_behind_security,
        resource_write_hints_limit_the_body,
        resource_write_hints_name_their_placeholders,
        test_suite_follows_the_routes,
        test_suite_compiles,
        test_suite_is_erlfmt_formatted,
        test_suite_reads_the_port_at_runtime
    ].

init_per_suite(Config) ->
    _ = application:load(nova),
    _ = application:load(rebar3_nova),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(rebar3_nova, router_module, rebar3_nova_test_router),
    Config.

end_per_suite(_Config) ->
    application:unset_env(rebar3_nova, router_module),
    _ = persistent_term:erase(nova_dispatch),
    _ = persistent_term:erase(nova_apps),
    ok.

%% A Nova request map has no `req` key; the old stubs matched on one and
%% crashed with function_clause.
controller_actions_take_a_nova_request(Config) ->
    Source = rebar3_nova_gen_controller:generate_content(gen_tags_controller, ?ACTIONS),
    Module = compile_source(Config, "gen_tags_controller", Source),
    Req = #{method => ~"GET", bindings => #{}},
    ?assertMatch({json, #{}}, Module:list(Req)),
    ?assertMatch({json, #{}}, Module:show(Req)),
    ?assertMatch({json, 201, #{}, #{}}, Module:create(Req)),
    ?assertMatch({json, #{}}, Module:update(Req)),
    ?assertEqual({status, 204}, Module:delete(Req)).

controller_is_erlfmt_formatted(_Config) ->
    assert_formatted(rebar3_nova_gen_controller:generate_content(gen_tags_controller, ?ACTIONS)),
    assert_formatted(rebar3_nova_gen_controller:generate_content(gen_tags_controller, [list])).

%% jesse, behind nova_json_schemas, knows drafts 3, 4 and 6 only.
resource_schema_is_draft_06(_Config) ->
    #{~"$schema" := Draft} = rebar3_nova_gen_resource:schema(),
    ?assertEqual(~"http://json-schema.org/draft-06/schema#", Draft).

resource_schema_does_not_require_id(_Config) ->
    ?assertMatch(#{~"required" := [~"name"]}, rebar3_nova_gen_resource:schema()).

resource_route_hints_use_funs(_Config) ->
    [Reads, Writes] = eval_hint(
        "[" ++ rebar3_nova_gen_resource:route_hints(blog, "tags", ?ACTIONS) ++ "]"
    ),
    Routes = maps:get(routes, Reads) ++ maps:get(routes, Writes),
    ?assertEqual(5, length(Routes)),
    [?assert(is_list(Path)) || {Path, _Fun, _Opts} <- Routes],
    [?assert(is_function(Fun, 1)) || {_Path, Fun, _Opts} <- Routes],
    ?assertMatch(
        [{"/tags", _, #{methods := [post], extra_state := #{json_schema := "schemas/tag.json"}}}],
        [R || {_, _, #{methods := [post]}} = R <- Routes]
    ),
    [{_, ListFun, _} | _] = Routes,
    ?assertEqual({module, blog_tags_controller}, erlang:fun_info(ListFun, module)).

%% Create, update and delete hints used to be printed bare, ready to paste
%% into a public group.
resource_write_hints_are_behind_security(_Config) ->
    [Reads, Writes] = eval_hint(
        "[" ++ rebar3_nova_gen_resource:route_hints(blog, "tags", ?ACTIONS) ++ "]"
    ),
    ?assertEqual(false, maps:is_key(security, Reads)),
    ?assertEqual([[get], [get]], [maps:get(methods, O) || {_, _, O} <- maps:get(routes, Reads)]),
    ?assertEqual(fun blog_auth_security:require_bearer/1, maps:get(security, Writes)),
    ?assertEqual(
        [[post], [put], [delete]], [maps:get(methods, O) || {_, _, O} <- maps:get(routes, Writes)]
    ),
    [OnlyWrites] = eval_hint(
        "[" ++ rebar3_nova_gen_resource:route_hints(blog, "tags", [create]) ++ "]"
    ),
    ?assert(maps:is_key(security, OnlyWrites)).

%% The hints must not send anyone to gen_auth for modules it does not make.
resource_write_hints_name_their_placeholders(_Config) ->
    Hints = rebar3_nova_gen_resource:route_hints(blog, "tags", ?ACTIONS),
    ?assertEqual(nomatch, string:find(Hints, "gen_auth")),
    ?assertNotEqual(nomatch, string:find(Hints, "Placeholders: replace require_bearer/1")),
    ?assertNotEqual(nomatch, string:find(Hints, "fail closed")).

%% A group's own plugins replace the global ones; without them the write
%% group inherits a body decoder that runs before security.
resource_write_hints_limit_the_body(_Config) ->
    Limited = [
        {pre_request, blog_body_limit, #{}},
        {pre_request, nova_request_plugin, #{decode_json_body => true}}
    ],
    [_Reads, Writes] = eval_hint(
        "[" ++ rebar3_nova_gen_resource:route_hints(blog, "tags", ?ACTIONS) ++ "]"
    ),
    ?assertEqual(Limited ++ [{pre_request, nova_json_schemas, #{}}], maps:get(plugins, Writes)),
    [DeleteOnly] = eval_hint(
        "[" ++ rebar3_nova_gen_resource:route_hints(blog, "tags", [delete]) ++ "]"
    ),
    ?assertEqual(Limited, maps:get(plugins, DeleteOnly)).

test_suite_follows_the_routes(_Config) ->
    rebar3_nova_test_router:set_routes([
        #{
            prefix => "/api",
            routes => [
                {"/tags", fun gen_tags_controller:list/1, #{methods => [get]}},
                {"/tags", fun gen_tags_controller:create/1, #{methods => [post]}},
                {"/tags/:id", fun gen_tags_controller:delete/1, #{methods => [delete]}},
                {"/other", fun other_controller:list/1, #{methods => [get]}}
            ]
        }
    ]),
    _ = persistent_term:erase(nova_dispatch),
    Dispatch = nova_router:compile([rebar3_nova]),
    ?assertEqual(
        [
            {~"delete", ~"/api/tags/:id", delete},
            {~"get", ~"/api/tags", list},
            {~"post", ~"/api/tags", create}
        ],
        rebar3_nova_gen_test:controller_routes(Dispatch, gen_tags_controller)
    ),
    Source = suite_source(),
    ?assertNotEqual(nomatch, string:find(Source, ~"request(get, \"/api/tags\")")),
    ?assertNotEqual(nomatch, string:find(Source, ~"request(delete, \"/api/tags/1\")")),
    ?assertNotEqual(nomatch, string:find(Source, ~"{{_, 201, _}, _, _}}, request(post")).

test_suite_compiles(Config) ->
    Module = compile_source(Config, "gen_tags_controller_SUITE", suite_source()),
    ?assertEqual(
        [test_list, test_create, test_delete],
        Module:all()
    ).

test_suite_is_erlfmt_formatted(_Config) ->
    assert_formatted(suite_source()).

test_suite_reads_the_port_at_runtime(_Config) ->
    Source = suite_source(),
    ?assertEqual(nomatch, string:find(Source, ~"8080/")),
    ?assertNotEqual(nomatch, string:find(Source, ~"cowboy_configuration")),
    ?assertNotEqual(
        nomatch, string:find(Source, ~"{ok, _} = application:ensure_all_started(blog)")
    ).

%%====================================================================
%% Helpers
%%====================================================================

suite_source() ->
    rebar3_nova_gen_test:generate_content("gen_tags_controller_SUITE", blog, [
        {~"get", ~"/api/tags", list},
        {~"post", ~"/api/tags", create},
        {~"delete", ~"/api/tags/:id", delete}
    ]).

compile_source(Config, Name, Source) ->
    File = ?config(priv_dir, Config) ++ "/" ++ Name ++ ".erl",
    ok = file:write_file(File, Source),
    {ok, Module, Binary} = compile:file(File, [binary, return_errors]),
    true = is_atom(Module) andalso is_binary(Binary),
    {module, Module} = code:load_binary(Module, File, Binary),
    Module.

assert_formatted(Source) ->
    Text = binary_to_list(Source),
    {ok, Formatted, _Warnings} = erlfmt:format_string(Text, []),
    ?assertEqual(Text, Formatted).

-spec eval_hint(string()) -> dynamic().
eval_hint(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression ++ "."),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:expr(Expr, []),
    Value.
