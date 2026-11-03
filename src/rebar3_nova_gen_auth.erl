-module(rebar3_nova_gen_auth).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_gen_auth_SUITE.
-export([files/1, render/2, instructions/1]).

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
        {short_desc, "Generate email/password token authentication on nova_auth"},
        {desc,
            "Generates a nova_auth config module, Kura user and token schemas, an accounts "
            "module, a bearer-token security function, a JSON controller (register, login, "
            "logout, me), a login limit, a body-limit plugin and a CT suite. Migrations come from the rebar3_kura compile hook."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    App = atom_to_list(rebar3_nova_utils:get_app_name(State)),
    AppDir = rebar3_nova_utils:get_app_dir(State),
    _ = [
        rebar3_nova_utils:write_file_if_not_exists(
            filename:join(AppDir, Path), render(Template, App)
        )
     || {Template, Path} <- files(App)
    ],
    print_instructions(App),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-doc "Each template under `priv/gen_auth` and the project path it is written to.".
-spec files(string()) -> [{string(), string()}].
files(App) ->
    [
        {"auth.erl.src", "src/" ++ App ++ "_auth.erl"},
        {"user.erl.src", "src/schemas/" ++ App ++ "_user.erl"},
        {"user_token.erl.src", "src/schemas/" ++ App ++ "_user_token.erl"},
        {"accounts.erl.src", "src/" ++ App ++ "_accounts.erl"},
        {"auth_security.erl.src", "src/" ++ App ++ "_auth_security.erl"},
        {"login_limit.erl.src", "src/" ++ App ++ "_login_limit.erl"},
        {"body_limit.erl.src", "src/plugins/" ++ App ++ "_body_limit.erl"},
        {"auth_controller.erl.src", "src/controllers/" ++ App ++ "_auth_controller.erl"},
        {"auth_SUITE.erl.src", "test/" ++ App ++ "_auth_SUITE.erl"}
    ].

-doc "A template with `{{app}}` replaced by the application name.".
-spec render(string(), string()) -> binary().
render(Template, App) ->
    {ok, Source} = file:read_file(filename:join([priv_dir(), "gen_auth", Template])),
    binary:replace(Source, ~"{{app}}", rebar3_nova_utils:to_binary(App), [global]).

priv_dir() ->
    case code:priv_dir(rebar3_nova) of
        Dir when is_list(Dir) -> Dir
    end.

print_instructions(App) ->
    rebar_api:info("~ts", [instructions(App)]).

-doc "The next steps printed after generating, for the application `App`.".
-spec instructions(string()) -> string().
instructions(App) ->
    Template =
        "\nAuthentication files generated. Next steps:\n\n"
        "1. Add the deps to rebar.config and to the applications list in APP.app.src:\n"
        "     {nova_auth, \"~> 0.3\"}, {kura, \"~> 2.25\"}, {kura_postgres, \"~> 1.1\"}\n"
        "   APP_auth expects a Kura repo module named APP_repo, as `rebar3 nova new --kura` "
        "generates.\n\n"
        "2. Let rebar3_kura write the users and user_tokens migrations from the schemas:\n"
        "     {project_plugins, [{rebar3_kura, \"~> 0.16\"}]}.\n"
        "     {provider_hooks, [{pre, [{compile, {kura, compile}}]}]}.\n"
        "   Run them with kura_migrator:migrate(APP_repo) as a release step, before the new "
        "code serves requests, not in APP_app:start/2, where several nodes would race.\n\n"
        "3. Authentication is bearer-only, so turn Nova's session cookie off in APP's env "
        "in sys.config:\n"
        "     {APP, [{use_sessions, false}]}\n\n"
        "4. Add the routes. Before anything reads the body, nova_auth_rate_limit allows 10 "
        "requests a minute per client address (per /64 for IPv6, see APP_login_limit:key/1) "
        "and APP_body_limit refuses bodies over 64 KiB. The client address is the TCP peer; "
        "behind a reverse proxy, list the proxies in nova_auth's env as "
        "{nova_auth, [{trusted_proxies, [{10, 0, 0, 2}, {{10, 1, 0, 0}, 16}]}]} so the client "
        "is read from x-forwarded-for. The bearer group sets plugins => [] so it does not "
        "inherit a global body-decoding plugin, which would read the body before the token "
        "is checked.\n"
        "     #{prefix => \"/api\",\n"
        "       plugins => [\n"
        "           {pre_request, nova_auth_rate_limit, #{key => {APP_login_limit, key}}},\n"
        "           {pre_request, APP_body_limit, #{max_bytes => 65536}},\n"
        "           {pre_request, nova_request_plugin, #{decode_json_body => true}}\n"
        "       ],\n"
        "       routes => [\n"
        "           {\"/users\", fun APP_auth_controller:register/1, #{methods => [post]}},\n"
        "           {\"/tokens\", fun APP_auth_controller:login/1, #{methods => [post]}}\n"
        "       ]},\n"
        "     #{prefix => \"/api\",\n"
        "       security => fun APP_auth_security:require_bearer/1,\n"
        "       plugins => [],\n"
        "       routes => [\n"
        "           {\"/tokens\", fun APP_auth_controller:logout/1, #{methods => [delete]}},\n"
        "           {\"/me\", fun APP_auth_controller:me/1, #{methods => [get]}}\n"
        "       ]}\n\n"
        "5. Protect your own API routes with security => fun APP_auth_security:require_bearer/1; "
        "auth_data in the request holds the user's id and email. Give each protected group "
        "its own plugins: Nova runs them before security, so an inherited global body "
        "decoder would read a body of any size from a request without a token.\n"
        "     #{prefix => \"/api\",\n"
        "       security => fun APP_auth_security:require_bearer/1,\n"
        "       plugins => [\n"
        "           {pre_request, APP_body_limit, #{}},\n"
        "           {pre_request, nova_request_plugin, #{decode_json_body => true}}\n"
        "       ],\n"
        "       routes => [\n"
        "           {\"/posts\", fun APP_post_controller:create/1, #{methods => [post]}}\n"
        "       ]}\n\n"
        "6. Registration takes email and password (at least 12 characters), and its 422 "
        "for a taken address tells anyone which emails have accounts; put confirmation "
        "by email in front of it if that matters. Login also allows 20 attempts per email "
        "in 15 minutes, whatever the address (APP_login_limit:check/1).\n"
        "   The suite in test/APP_auth_SUITE.erl registers users in the database it runs "
        "against.\n",
    replace_app(Template, App).

-spec replace_app(string(), string()) -> string().
replace_app("APP" ++ Rest, App) -> App ++ replace_app(Rest, App);
replace_app([C | Rest], App) -> [C | replace_app(Rest, App)];
replace_app([], _App) -> [].
