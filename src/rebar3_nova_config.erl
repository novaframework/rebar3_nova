-module(rebar3_nova_config).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_commands_SUITE.
-export([config_sections/2]).

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
    Config = rebar3_nova_utils:load_sys_config(State),
    AppName = rebar3_nova_utils:get_app_name(State),
    io:format("~n=== Nova Configuration ===~n"),
    lists:foreach(fun print_section/1, config_sections(Config, AppName)),
    case proplists:get_value(bootstrap_application, app_env(Config, nova)) of
        undefined ->
            rebar_api:warn("~nbootstrap_application is not set in Nova config!", []);
        _ ->
            ok
    end,
    {ok, State}.

-type value() :: {set, term()} | {default, term()} | missing | {misplaced, term(), atom()}.

-doc """
The settings Nova reads, grouped by the application env each one lives
in: `nova` itself, or the bootstrap application (read with
`nova:get_env/2`).
""".
-spec config_sections([{atom(), term()}], atom()) -> [{atom(), [{atom(), value()}]}].
config_sections(Config, AppName) ->
    NovaEnv = app_env(Config, nova),
    Bootstrap =
        case proplists:get_value(bootstrap_application, NovaEnv) of
            B when is_atom(B), B =/= undefined -> B;
            _ -> AppName
        end,
    BootstrapEnv = app_env(Config, Bootstrap),
    NovaKeys = [{Key, lookup(Key, Default, NovaEnv)} || {Key, Default} <- nova_keys()],
    BootstrapKeys = [
        {Key, bootstrap_value(Key, Default, BootstrapEnv, NovaEnv, Bootstrap)}
     || {Key, Default} <- bootstrap_keys()
    ],
    [{nova, NovaKeys}, {Bootstrap, BootstrapKeys}].

nova_keys() ->
    [
        {bootstrap_application, required},
        {environment, dev},
        {cowboy_configuration, #{port => 8080}},
        {plugins, []},
        {use_stacktrace, false},
        {render_error_pages, true},
        {use_strict_routing, false},
        {session_manager, nova_session_ets},
        {dispatch_backend, persistent_term}
    ].

bootstrap_keys() ->
    [
        {json_lib, thoas},
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

print_section({App, Keys}) ->
    io:format("~n  {~p, [...]}~n", [App]),
    lists:foreach(fun print_key/1, Keys).

print_key({Key, missing}) ->
    io:format("    ~-25s *** MISSING (required) ***~n", [Key]);
print_key({Key, {default, Default}}) ->
    io:format("    ~-25s ~p (default)~n", [Key, Default]);
print_key({Key, {set, Value}}) ->
    io:format("    ~-25s ~p~n", [Key, Value]);
print_key({Key, {misplaced, Value, Bootstrap}}) ->
    io:format("    ~-25s ~p under nova, but Nova reads this key from ~p~n", [Key, Value, Bootstrap]).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
