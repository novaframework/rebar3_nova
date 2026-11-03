-module(rebar3_nova_gen_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kura/include/kura.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    controller_actions_take_a_nova_request/1,
    controller_is_erlfmt_formatted/1,
    resource_schema_is_draft_06/1,
    resource_schema_does_not_require_id/1,
    resource_route_hints_use_funs/1,
    resource_write_hints_are_behind_security/1,
    resource_write_hints_limit_the_body/1,
    test_suite_follows_the_routes/1,
    test_suite_compiles/1,
    test_suite_is_erlfmt_formatted/1,
    test_suite_reads_the_port_at_runtime/1,
    auth_files_are_valid_formatted_erlang/1,
    auth_uses_nova_auth_not_bcrypt_or_thoas/1,
    auth_writes_no_migration/1,
    auth_names_follow_the_app/1,
    auth_instructions_pin_nova_auth_to_0_2/1,
    auth_rate_limit_runs_before_the_body_is_decoded/1,
    auth_instructions_cover_sessions_enumeration_and_proxies/1,
    auth_protected_group_example_limits_the_body/1,
    auth_suite_builds_binaries_with_sigils/1,
    auth_token_schema_has_the_refresh_fields/1,
    auth_registration_needs_a_matching_confirmation/1,
    auth_changesets_scrub_the_password/1,
    auth_email_change_resets_confirmed_at/1
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
        test_suite_follows_the_routes,
        test_suite_compiles,
        test_suite_is_erlfmt_formatted,
        test_suite_reads_the_port_at_runtime,
        auth_files_are_valid_formatted_erlang,
        auth_uses_nova_auth_not_bcrypt_or_thoas,
        auth_writes_no_migration,
        auth_names_follow_the_app,
        auth_instructions_pin_nova_auth_to_0_2,
        auth_rate_limit_runs_before_the_body_is_decoded,
        auth_instructions_cover_sessions_enumeration_and_proxies,
        auth_protected_group_example_limits_the_body,
        auth_suite_builds_binaries_with_sigils,
        auth_token_schema_has_the_refresh_fields,
        auth_registration_needs_a_matching_confirmation,
        auth_changesets_scrub_the_password,
        auth_email_change_resets_confirmed_at
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

auth_files_are_valid_formatted_erlang(_Config) ->
    [assert_formatted(rebar3_nova_gen_auth:render(T, "blog")) || {T, _} <- auth_files()].

auth_uses_nova_auth_not_bcrypt_or_thoas(_Config) ->
    Sources = [rebar3_nova_gen_auth:render(T, "blog") || {T, _} <- auth_files()],
    [?assertEqual(nomatch, binary:match(S, [~"bcrypt", ~"thoas", ~"citext"])) || S <- Sources],
    All = iolist_to_binary(Sources),
    ?assertNotEqual(nomatch, binary:match(All, ~"nova_auth_password:hash")),
    ?assertNotEqual(nomatch, binary:match(All, ~"nova_auth_accounts:authenticate")),
    ?assertNotEqual(nomatch, binary:match(All, ~"-behaviour(nova_auth).")).

auth_writes_no_migration(_Config) ->
    ?assertEqual([], [P || {_, P} <- auth_files(), string:find(P, "migration") =/= nomatch]).

auth_names_follow_the_app(_Config) ->
    Paths = [P || {_, P} <- auth_files()],
    ?assert(lists:member("src/blog_auth.erl", Paths)),
    ?assert(lists:member("src/schemas/blog_user.erl", Paths)),
    ?assert(lists:member("src/controllers/blog_auth_controller.erl", Paths)),
    [
        ?assertEqual(nomatch, binary:match(rebar3_nova_gen_auth:render(T, "blog"), ~"{{app}}"))
     || {T, _} <- auth_files()
    ],
    [
        ?assertNotEqual(
            nomatch,
            binary:match(
                rebar3_nova_gen_auth:render(T, "blog"),
                iolist_to_binary(["-module(", filename:basename(P, ".erl"), ")."])
            )
        )
     || {T, P} <- auth_files()
    ].

auth_instructions_pin_nova_auth_to_0_2(_Config) ->
    Text = rebar3_nova_gen_auth:instructions("blog"),
    ?assertNotEqual(nomatch, string:find(Text, "{nova_auth, \"~> 0.2.0\"}")),
    ?assertEqual(nomatch, string:find(Text, "{nova_auth, \"~> 0.2\"}")).

%% A limiter after nova_request_plugin decodes every flood request's body
%% before refusing it.
auth_rate_limit_runs_before_the_body_is_decoded(_Config) ->
    Instructions = rebar3_nova_gen_auth:instructions("blog"),
    Start = string:str(Instructions, "4. Add the routes"),
    Text = string:substr(Instructions, Start, string:str(Instructions, "\n\n5.") - Start),
    Limiter = string:str(Text, "{pre_request, blog_rate_limit, #{}}"),
    BodyLimit = string:str(Text, "{pre_request, blog_body_limit, #{max_bytes => 65536}}"),
    Decoder = string:str(Text, "{pre_request, nova_request_plugin"),
    ?assert(Limiter > 0),
    ?assert(Limiter < BodyLimit),
    ?assert(BodyLimit < Decoder),
    ?assertEqual(1, length(string:split(Text, "nova_request_plugin", all)) - 1),
    ?assertNotEqual(nomatch, string:find(Text, "plugins => [],")).

auth_instructions_cover_sessions_enumeration_and_proxies(_Config) ->
    Text = rebar3_nova_gen_auth:instructions("blog"),
    [
        ?assertNotEqual(nomatch, string:find(Text, Needle), Needle)
     || Needle <- [
            "{blog, [{use_sessions, false}]}",
            "tells anyone which emails have accounts",
            "{trusted_proxies, [",
            "nova_auth_rate_limit:client_ip/1",
            "20 attempts per email"
        ]
    ].

auth_protected_group_example_limits_the_body(_Config) ->
    Text = rebar3_nova_gen_auth:instructions("blog"),
    Start = string:str(Text, "5. Protect"),
    Step = string:substr(Text, Start, string:str(Text, "\n\n6.") - Start),
    Group = eval_hint(string:substr(Step, string:str(Step, "#{prefix"))),
    ?assertEqual(fun blog_auth_security:require_bearer/1, maps:get(security, Group)),
    ?assertEqual(
        [
            {pre_request, blog_body_limit, #{}},
            {pre_request, nova_request_plugin, #{decode_json_body => true}}
        ],
        maps:get(plugins, Group)
    ).

auth_suite_builds_binaries_with_sigils(_Config) ->
    ?assertEqual(nomatch, binary:match(render_auth("auth_SUITE.erl.src"), ~"<<\"")).

auth_token_schema_has_the_refresh_fields(Config) ->
    Token = compile_source(Config, "blog_user_token", render_auth("user_token.erl.src")),
    Types = #{F#kura_field.name => F#kura_field.type || F <- Token:fields()},
    ?assertMatch(#{family_id := string, used_at := utc_datetime}, Types).

auth_registration_needs_a_matching_confirmation(Config) ->
    User = user_schema(Config),
    Valid = registration(User, ~"a@example.com", ~"correct horse battery"),
    ?assert(Valid#kura_changeset.valid),
    ?assertEqual(
        ~"hashed:correct horse battery", kura_changeset:get_change(Valid, hashed_password)
    ),
    Mismatch = User:registration_changeset(#{}, #{
        ~"email" => ~"a@example.com",
        ~"password" => ~"correct horse battery",
        ~"password_confirmation" => ~"something else"
    }),
    ?assertNot(Mismatch#kura_changeset.valid),
    ?assertMatch([{password_confirmation, _}], Mismatch#kura_changeset.errors),
    ?assertEqual(undefined, kura_changeset:get_change(Mismatch, hashed_password)).

%% A changeset comes back in {error, CS} and can reach a log or crash report.
auth_changesets_scrub_the_password(Config) ->
    User = user_schema(Config),
    Pass = ~"correct horse battery",
    assert_no_plaintext(registration(User, ~"a@example.com", Pass)),
    assert_no_plaintext(registration(User, ~"not an email", ~"short")),
    assert_no_plaintext(
        User:registration_changeset(#{}, #{
            ~"email" => ~"a@example.com", ~"password" => Pass, ~"password_confirmation" => ~"x"
        })
    ),
    Change = User:password_changeset(#{id => 1, email => ~"a@example.com"}, #{
        ~"password" => ~"a new long passphrase",
        ~"password_confirmation" => ~"a new long passphrase"
    }),
    ?assert(Change#kura_changeset.valid),
    ?assertEqual(
        ~"hashed:a new long passphrase", kura_changeset:get_change(Change, hashed_password)
    ),
    assert_no_plaintext(Change).

auth_email_change_resets_confirmed_at(Config) ->
    User = user_schema(Config),
    Data = #{id => 1, email => ~"old@b.com", confirmed_at => {{2026, 1, 1}, {0, 0, 0}}},
    Changed = User:email_changeset(Data, #{~"email" => ~"New@B.com"}),
    ?assert(Changed#kura_changeset.valid),
    ?assertEqual(
        #{email => ~"new@b.com", confirmed_at => undefined}, Changed#kura_changeset.changes
    ),
    Same = User:email_changeset(Data, #{~"email" => ~"OLD@b.com"}),
    ?assertEqual(#{}, Same#kura_changeset.changes).

%%====================================================================
%% Helpers
%%====================================================================

auth_files() ->
    rebar3_nova_gen_auth:files("blog").

render_auth(Template) ->
    rebar3_nova_gen_auth:render(Template, "blog").

user_schema(Config) ->
    _ = compile_source(Config, "nova_auth_password", [
        "-module(nova_auth_password).\n-export([hash/1]).\n",
        "hash(Password) -> <<\"hashed:\", Password/binary>>.\n"
    ]),
    compile_source(Config, "blog_user", render_auth("user.erl.src")).

registration(User, Email, Password) ->
    User:registration_changeset(#{}, #{
        ~"email" => Email, ~"password" => Password, ~"password_confirmation" => Password
    }).

assert_no_plaintext(#kura_changeset{changes = Changes, params = Params}) ->
    Secret = [password, password_confirmation, ~"password", ~"password_confirmation"],
    ?assertEqual([], [K || K <- Secret, maps:is_key(K, Changes)]),
    ?assertEqual([], [K || K <- Secret, maps:is_key(K, Params)]).

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
