-module(rebar3_nova_config).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_commands_SUITE.
-export([config_sections/2, config_sections/3, format_section/1]).

-define(PROVIDER, config).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova config"},
        {opts, []},
        {short_desc, "Show Nova application configuration"},
        {desc, "Displays all Nova configuration keys with their values or defaults"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_paths:set_paths([deps, plugins], State),
    Config = rebar3_nova_utils:load_sys_config(State),
    AppName = rebar3_nova_utils:get_app_name(State),
    io:format("~n=== Nova Configuration ===~n"),
    io:put_chars([format_section(Section) || Section <- config_sections(Config, AppName)]),
    case proplists:get_value(bootstrap_application, app_env(Config, nova)) of
        undefined ->
            rebar_api:warn("~nbootstrap_application is not set in Nova config!", []);
        _ ->
            ok
    end,
    {ok, State}.

-type value() ::
    {set, term()}
    | {default, term()}
    | missing
    | {misplaced, term(), atom()}
    | secret
    | {removed, atom()}.

-doc """
The settings Nova reads, grouped by the application env each one lives
in: `nova` itself, or the bootstrap application (read with
`nova:get_env/2`). The settings a later Nova than 0.18 checks at boot are
listed when the project's Nova is one.
""".
-spec config_sections([{atom(), term()}], atom()) -> [{atom(), [{atom(), value()}]}].
config_sections(Config, AppName) ->
    config_sections(Config, AppName, rebar3_nova_utils:settings_module()).

-doc """
As `config_sections/2`, for a Nova whose settings module is `Settings`
(`undefined` for one without). A set `secret_key_base` is listed as
`secret`, never with its value.
""".
-spec config_sections([{atom(), term()}], atom(), module() | undefined) ->
    [{atom(), [{atom(), value()}]}].
config_sections(Config, AppName, Settings) ->
    NovaEnv = app_env(Config, nova),
    Bootstrap =
        case proplists:get_value(bootstrap_application, NovaEnv) of
            B when is_atom(B), B =/= undefined -> B;
            _ -> AppName
        end,
    BootstrapEnv = app_env(Config, Bootstrap),
    NovaKeys = [{Key, lookup(Key, Default, NovaEnv)} || {Key, Default} <- nova_keys(NovaEnv)],
    BootstrapKeys = [
        {Key, bootstrap_value(Key, Default, BootstrapEnv, NovaEnv, Bootstrap)}
     || {Key, Default} <- bootstrap_keys()
    ],
    [{nova, NovaKeys ++ checked_settings(Settings, NovaEnv)}, {Bootstrap, BootstrapKeys}].

checked_settings(undefined, _NovaEnv) ->
    [];
checked_settings(Settings, NovaEnv) ->
    [
        {Key, checked_value(Key, lookup(Key, Default, NovaEnv))}
     || {Key, Default} <- rebar3_nova_utils:checked_settings()
    ] ++
        [
            {Key, {removed, Replacement}}
         || {Key, Replacement} <- Settings:removed(), proplists:is_defined(Key, NovaEnv)
        ].

checked_value(secret_key_base, {set, _Secret}) -> secret;
checked_value(_Key, Value) -> Value.

nova_keys(NovaEnv) ->
    [
        {bootstrap_application, required},
        {environment, rebar3_nova_utils:default_environment()},
        listeners_key(NovaEnv),
        {plugins, []},
        {use_stacktrace, false},
        {render_error_pages, true},
        {use_strict_routing, false},
        {session_manager, nova_session_ets},
        {dispatch_backend, persistent_term}
    ].

%% Nova 1.0 reads `cowboy_configuration` only when `listeners` is not set.
listeners_key(NovaEnv) ->
    case
        {
            proplists:is_defined(listeners, NovaEnv),
            proplists:is_defined(cowboy_configuration, NovaEnv)
        }
    of
        {false, true} -> {cowboy_configuration, #{port => 8080}};
        _ -> {listeners, #{default => #{port => 8080}}}
    end.

bootstrap_keys() ->
    [
        {json_lib, rebar3_nova_utils:default_json_lib()},
        {use_sessions, true}
    ].

lookup(Key, Default, Env) ->
    case proplists:lookup(Key, Env) of
        {Key, Value} -> {set, Value};
        none when Default =:= required -> missing;
        none -> {default, Default}
    end.

bootstrap_value(Key, Default, BootstrapEnv, NovaEnv, Bootstrap) ->
    case {lookup(Key, Default, BootstrapEnv), proplists:lookup(Key, NovaEnv)} of
        {{default, _}, {Key, Value}} -> {misplaced, Value, Bootstrap};
        {Found, _} -> Found
    end.

app_env(Config, App) ->
    case proplists:get_value(App, Config, []) of
        Env when is_list(Env) -> Env;
        _ -> []
    end.

-doc "A section of `config_sections/2` as the lines the command prints.".
-spec format_section({atom(), [{atom(), value()}]}) -> io_lib:chars().
format_section({App, Keys}) ->
    [io_lib:format("~n  {~p, [...]}~n", [App]), [format_key(Key) || Key <- Keys]].

format_key({Key, missing}) ->
    io_lib:format("    ~-25s *** MISSING (required) ***~n", [Key]);
format_key({Key, {default, Default}}) ->
    io_lib:format("    ~-25s ~p (default)~n", [Key, Default]);
format_key({Key, {set, Value}}) ->
    io_lib:format("    ~-25s ~p~n", [Key, Value]);
format_key({Key, secret}) ->
    io_lib:format("    ~-25s set (not shown)~n", [Key]);
format_key({Key, {removed, Replacement}}) ->
    io_lib:format("    ~-25s no longer a setting: Nova does not start with it, use ~p~n", [
        Key, Replacement
    ]);
format_key({Key, {misplaced, Value, Bootstrap}}) ->
    io_lib:format("    ~-25s ~p under nova, but Nova reads this key from ~p~n", [
        Key, Value, Bootstrap
    ]).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
