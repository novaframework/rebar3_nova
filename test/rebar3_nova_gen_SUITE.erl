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
    test_suite_reads_the_port_at_runtime/1,
    auth_files_use_otp_json/1,
    resource_write_hints_say_what_the_security_function_reads/1,
    route_table_takes_the_hinted_group/1,
    auth_controllers_sign_in_where_nova_can/1,
    auth_controllers_store_the_token_on_an_older_nova/1,
    auth_logout_deletes_the_session/1,
    auth_session_write_follows_nova/1,
    auth_route_hints_are_two_groups/1
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
        test_suite_reads_the_port_at_runtime,
        auth_files_use_otp_json,
        resource_write_hints_say_what_the_security_function_reads,
        route_table_takes_the_hinted_group,
        auth_controllers_sign_in_where_nova_can,
        auth_controllers_store_the_token_on_an_older_nova,
        auth_logout_deletes_the_session,
        auth_session_write_follows_nova,
        auth_route_hints_are_two_groups
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

%% Nova no longer depends on thoas, so generated code that called it would
%% not run. OTP's json is there on every Nova.
auth_files_use_otp_json(Config) ->
    AppDir = filename:join(proplists:get_value(priv_dir, Config), "gen_auth_app"),
    ok = filelib:ensure_path(filename:join(AppDir, "src")),
    ok = filelib:ensure_path(filename:join(AppDir, "test")),
    rebar3_nova_gen_auth:generate_auth("blog", AppDir),
    rebar3_nova_gen_auth:generate_test_suite("blog", AppDir),
    Files = [
        filename:join([AppDir, "src", "blog_auth.erl"]),
        filename:join([AppDir, "test", "blog_auth_SUITE.erl"])
    ],
    lists:foreach(
        fun(File) ->
            {ok, Source} = file:read_file(File),
            ?assertEqual(nomatch, binary:match(Source, ~"thoas"), File),
            ?assertMatch({ok, _, _}, compile:file(File, [binary, return_errors]), File)
        end,
        Files
    ).

%% A Nova that runs the body decoder behind the security function warns
%% about a group that does not say whether the function reads the body.
resource_write_hints_say_what_the_security_function_reads(_Config) ->
    Hints = rebar3_nova_gen_resource:route_hints(blog, "tags", ?ACTIONS),
    [Reads, Writes] = eval_hint("[" ++ Hints ++ "]"),
    ?assertEqual(false, maps:get(security_reads_body, Writes)),
    ?assertNot(maps:is_key(security_reads_body, Reads)),
    ?assertEqual(nomatch, string:find(Hints, "Plugins run before security")).

route_table_takes_the_hinted_group(_Config) ->
    [_Reads, Writes] = eval_hint(
        "[" ++ rebar3_nova_gen_resource:route_hints(blog, "tags", ?ACTIONS) ++ "]"
    ),
    rebar3_nova_test_router:set_routes([
        Writes#{
            security := fun test_security:allow/1,
            plugins := [{pre_request, nova_request_plugin, #{decode_json_body => true}}]
        }
    ]),
    {ok, Dispatch} = rebar3_nova_utils:route_table([rebar3_nova]),
    ?assertEqual(
        [~"/tags", ~"/tags/:id"],
        lists:usort([Path || {Path, _, _} <- rebar3_nova_dispatch:routes(Dispatch)])
    ).

auth_controllers_sign_in_where_nova_can(Config) ->
    Sources = auth_controllers(Config, "sign_in_app", sign_in),
    ?assertEqual(
        4, occurrences(Sources, ~"nova_session:sign_in(Req, <<\"session_token\">>, Token)")
    ),
    ?assertEqual(0, occurrences(Sources, ~"nova_session:set(")).

auth_controllers_store_the_token_on_an_older_nova(Config) ->
    Sources = auth_controllers(Config, "set_app", set),
    ?assertEqual(
        4, occurrences(Sources, ~"ok = nova_session:set(Req, <<\"session_token\">>, Token)")
    ),
    ?assertEqual(0, occurrences(Sources, ~"sign_in")).

%% Deleting the one key left the session, and whatever else it held, behind.
auth_logout_deletes_the_session(Config) ->
    Sources = auth_controllers(Config, "logout_app", sign_in),
    ?assertEqual(1, occurrences(Sources, ~"nova_session:delete(Req)")),
    ?assertEqual(0, occurrences(Sources, ~"nova_session:delete(Req, ")).

auth_session_write_follows_nova(_Config) ->
    _ = code:ensure_loaded(nova_session),
    Expected =
        case erlang:function_exported(nova_session, sign_in, 3) of
            true -> sign_in;
            false -> set
        end,
    ?assertEqual(Expected, rebar3_nova_gen_auth:session_write()).

auth_route_hints_are_two_groups(_Config) ->
    [Public, Protected] = eval_hint("[" ++ rebar3_nova_gen_auth:route_hints("blog") ++ "]"),
    ?assertEqual(false, maps:get(security, Public)),
    ?assertNot(maps:is_key(security_reads_body, Public)),
    ?assertEqual(fun blog_auth:require_authenticated/1, maps:get(security, Protected)),
    ?assertEqual(false, maps:get(security_reads_body, Protected)),
    ?assertEqual(2, length(maps:get(routes, Public))),
    ?assertEqual(4, length(maps:get(routes, Protected))).

auth_controllers(Config, Name, SessionWrite) ->
    AppDir = ?config(priv_dir, Config) ++ "/" ++ Name,
    ok = filelib:ensure_path(AppDir ++ "/src"),
    rebar3_nova_gen_auth:generate_controllers("blog", AppDir, SessionWrite),
    Files = [
        AppDir ++ "/src/controllers/blog_" ++ Controller ++ "_controller.erl"
     || Controller <- ["session", "registration", "user"]
    ],
    iolist_to_binary([
        begin
            ?assertMatch({ok, _, _}, compile:file(File, [binary, return_errors]), File),
            {ok, Source} = file:read_file(File),
            Source
        end
     || File <- Files
    ]).

occurrences(Source, Pattern) ->
    length(binary:matches(Source, Pattern)).
