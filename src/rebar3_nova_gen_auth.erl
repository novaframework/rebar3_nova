-module(rebar3_nova_gen_auth).

-export([init/1, do/1, run/1, run/2, format_error/1]).

-define(PROVIDER, gen_auth).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova gen_auth"},
        {opts, []},
        {short_desc,
            "Generate email/password authentication (deprecated: use `rebar3 nova gen auth`)"},
        {desc, "Generates schemas, controllers, and context modules for email/password auth"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:warn("gen_auth is deprecated. Use `rebar3 nova gen auth` instead.", []),
    run(State).

-spec run(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
run(State) ->
    run(State, []).

-spec run(rebar_state:t(), proplists:proplist()) -> {ok, rebar_state:t()} | {error, string()}.
run(State, _Args) ->
    AppName = rebar3_nova_utils:get_app_name(State),
    AppDir = rebar3_nova_utils:get_app_dir(State),
    App = atom_to_list(AppName),
    Ctx = auth_context(App),
    generate_migration(AppDir, Ctx),
    generate_user_schema(AppDir, Ctx),
    generate_user_token_schema(AppDir, Ctx),
    generate_accounts(AppDir, Ctx),
    generate_auth(AppDir, Ctx),
    generate_session_controller(AppDir, Ctx),
    generate_registration_controller(AppDir, Ctx),
    generate_user_controller(AppDir, Ctx),
    generate_test_suite(AppDir, Ctx),
    print_instructions(App),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%======================================================================
%% Internal: build shared context
%%======================================================================

auth_context(App) ->
    #{
        app => App,
        user_mod => App ++ "_user",
        token_mod => App ++ "_user_token",
        accounts => App ++ "_accounts",
        repo => App ++ "_repo"
    }.

%%======================================================================
%% Internal: generators
%%======================================================================

generate_migration(AppDir, Ctx) ->
    TS = rebar3_nova_utils:timestamp(),
    Mod = "m" ++ TS ++ "_create_auth_tables",
    FileName = filename:join([AppDir, "src", "migrations", Mod ++ ".erl"]),
    Content = rebar3_nova_utils:render_template(
        ["gen", "auth", "migration.erl.mustache"],
        Ctx#{mod => Mod}
    ),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_user_schema(AppDir, #{user_mod := Mod} = Ctx) ->
    FileName = filename:join([AppDir, "src", "schemas", Mod ++ ".erl"]),
    Content = rebar3_nova_utils:render_template(
        ["gen", "auth", "user_schema.erl.mustache"],
        Ctx#{mod => Mod}
    ),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_user_token_schema(AppDir, #{token_mod := Mod} = Ctx) ->
    FileName = filename:join([AppDir, "src", "schemas", Mod ++ ".erl"]),
    Content = rebar3_nova_utils:render_template(
        ["gen", "auth", "user_token_schema.erl.mustache"],
        Ctx#{mod => Mod}
    ),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_accounts(AppDir, #{accounts := Mod} = Ctx) ->
    FileName = filename:join([AppDir, "src", Mod ++ ".erl"]),
    Content = rebar3_nova_utils:render_template(
        ["gen", "auth", "accounts.erl.mustache"],
        Ctx#{mod => Mod}
    ),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_auth(AppDir, #{app := App} = Ctx) ->
    Mod = App ++ "_auth",
    FileName = filename:join([AppDir, "src", Mod ++ ".erl"]),
    Content = rebar3_nova_utils:render_template(
        ["gen", "auth", "auth.erl.mustache"],
        Ctx#{mod => Mod}
    ),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_session_controller(AppDir, #{app := App} = Ctx) ->
    Mod = App ++ "_session_controller",
    FileName = filename:join([AppDir, "src", "controllers", Mod ++ ".erl"]),
    Content = rebar3_nova_utils:render_template(
        ["gen", "auth", "session_controller.erl.mustache"],
        Ctx#{mod => Mod}
    ),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_registration_controller(AppDir, #{app := App} = Ctx) ->
    Mod = App ++ "_registration_controller",
    FileName = filename:join([AppDir, "src", "controllers", Mod ++ ".erl"]),
    Content = rebar3_nova_utils:render_template(
        ["gen", "auth", "registration_controller.erl.mustache"],
        Ctx#{mod => Mod}
    ),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_user_controller(AppDir, #{app := App} = Ctx) ->
    Mod = App ++ "_user_controller",
    FileName = filename:join([AppDir, "src", "controllers", Mod ++ ".erl"]),
    Content = rebar3_nova_utils:render_template(
        ["gen", "auth", "user_controller.erl.mustache"],
        Ctx#{mod => Mod}
    ),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_test_suite(AppDir, #{app := App} = Ctx) ->
    Suite = App ++ "_auth_SUITE",
    FileName = filename:join([AppDir, "test", Suite ++ ".erl"]),
    Content = rebar3_nova_utils:render_template(
        ["gen", "auth", "auth_test_suite.erl.mustache"],
        Ctx#{suite => Suite}
    ),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% Print instructions
%%======================================================================

print_instructions(App) ->
    AuthMod = App ++ "_auth",
    SessionCtrl = App ++ "_session_controller",
    RegCtrl = App ++ "_registration_controller",
    UserCtrl = App ++ "_user_controller",
    rebar_api:info("~n==> Authentication files generated successfully!~n", []),
    rebar_api:info("Next steps:~n", []),
    rebar_api:info("1. Add kura and bcrypt to your deps in rebar.config:~n", []),
    rebar_api:info("   {deps, [..., kura, bcrypt]}.~n~n", []),
    rebar_api:info("2. Add these routes to your router:~n", []),
    rebar_api:info("   %% Public routes~n", []),
    rebar_api:info(
        "   #{prefix => <<\"/api\">>,~n"
        "     security => false,~n"
        "     plugins => [{pre_request, nova_request_plugin,~n"
        "                  #{decode_json_body => true}}],~n"
        "     routes => [~n"
        "       {<<\"/register\">>, fun ~s:create/1, #{methods => [post]}},~n"
        "       {<<\"/login\">>, fun ~s:create/1, #{methods => [post]}}~n"
        "     ]}~n",
        [RegCtrl, SessionCtrl]
    ),
    rebar_api:info("   %% Protected routes~n", []),
    rebar_api:info(
        "   #{prefix => <<\"/api\">>,~n"
        "     security => fun ~s:require_authenticated/1,~n"
        "     plugins => [{pre_request, nova_request_plugin,~n"
        "                  #{decode_json_body => true}}],~n"
        "     routes => [~n"
        "       {<<\"/logout\">>, fun ~s:delete/1, #{methods => [delete]}},~n"
        "       {<<\"/me\">>, fun ~s:show/1, #{methods => [get]}},~n"
        "       {<<\"/me/password\">>, fun ~s:update_password/1, #{methods => [put]}},~n"
        "       {<<\"/me/email\">>, fun ~s:update_email/1, #{methods => [put]}}~n"
        "     ]}~n",
        [AuthMod, SessionCtrl, UserCtrl, UserCtrl, UserCtrl]
    ),
    rebar_api:info("3. Add src/schemas and src/migrations to src_dirs in rebar.config:~n", []),
    rebar_api:info(
        "   {src_dirs, [\"src\", \"src/controllers\", \"src/schemas\", \"src/migrations\"]}.~n~n",
        []
    ),
    rebar_api:info("4. Run the migration:~n", []),
    rebar_api:info("   rebar3 kura migrate~n~n", []),
    rebar_api:info(
        "5. Ensure nova_request_plugin with decode_json_body is configured~n"
        "   (either globally in sys.config or per route group as shown above).~n",
        []
    ).
