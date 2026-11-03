-module(rebar3_nova_gen_auth).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_gen_SUITE.
-export([files/1, render/2]).

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
            "logout, me), a rate-limit plugin and a CT suite. Migrations come from the rebar3_kura compile hook."}
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
        {"rate_limit.erl.src", "src/plugins/" ++ App ++ "_rate_limit.erl"},
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
    rebar_api:info(
        "~nAuthentication files generated. Next steps:~n~n"
        "1. Add the deps to rebar.config and to the applications list in ~s.app.src:~n"
        "     {nova_auth, \"~~> 0.2\"}, {kura, \"~~> 2.25\"}, {kura_postgres, \"~~> 1.1\"}~n"
        "   ~s_auth expects a Kura repo module named ~s_repo, as `rebar3 nova new --kura` "
        "generates.~n~n"
        "2. Let rebar3_kura write the users and user_tokens migrations from the schemas:~n"
        "     {project_plugins, [{rebar3_kura, \"~~> 0.16\"}]}.~n"
        "     {provider_hooks, [{pre, [{compile, {kura, compile}}]}]}.~n"
        "   then run them, for example with kura_migrator:migrate(~s_repo) at boot.~n~n"
        "3. Add the routes. ~s_rate_limit limits login and registration to 10 requests a "
        "minute per client IP; behind a proxy, change its client_ip/1.~n"
        "     #{prefix => \"/api\",~n"
        "       plugins => [~n"
        "           {pre_request, nova_request_plugin, #{decode_json_body => true}},~n"
        "           {pre_request, ~s_rate_limit, #{}}~n"
        "       ],~n"
        "       routes => [~n"
        "           {\"/users\", fun ~s_auth_controller:register/1, #{methods => [post]}},~n"
        "           {\"/tokens\", fun ~s_auth_controller:login/1, #{methods => [post]}}~n"
        "       ]},~n"
        "     #{prefix => \"/api\",~n"
        "       security => fun ~s_auth_security:require_bearer/1,~n"
        "       plugins => [{pre_request, nova_request_plugin, #{decode_json_body => true}}],~n"
        "       routes => [~n"
        "           {\"/tokens\", fun ~s_auth_controller:logout/1, #{methods => [delete]}},~n"
        "           {\"/me\", fun ~s_auth_controller:me/1, #{methods => [get]}}~n"
        "       ]}~n~n"
        "4. Protect your own API routes with security => fun ~s_auth_security:require_bearer/1; "
        "the user is in the request under auth_data.~n"
        "   The suite in test/~s_auth_SUITE.erl registers users in the database it runs "
        "against.~n",
        lists:duplicate(13, App)
    ).
