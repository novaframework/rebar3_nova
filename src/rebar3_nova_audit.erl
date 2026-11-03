-module(rebar3_nova_audit).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_dispatch_SUITE and rebar3_nova_commands_SUITE.
-export([collect_routes/1, classify_findings/1, audit/3, run/2, nova_apps/2, consult_config/2]).

-include("nova_router.hrl").

-define(PROVIDER, audit).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova audit --env prod --strict"},
        {opts, [
            {env, $e, "env", {atom, dev},
                "Nova environment the routes are compiled for, as routes/1 sees it"},
            {config, $c, "config", string,
                "sys.config or .config.src to read nova_apps from (default: the shell config)"},
            {strict, $s, "strict", {boolean, false}, "Fail when there are warnings"}
        ]},
        {short_desc, "Audit route security configuration"},
        {desc,
            "Checks the routes of the application and the nova_apps in its sys.config for "
            "security issues like unsecured mutations and wildcard methods."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    App = rebar3_nova_utils:get_app_name(State),
    {Args, _} = rebar_state:command_parsed_args(State),
    {ConfigFile, Explicit} =
        case proplists:get_value(config, Args) of
            undefined -> {rebar3_nova_utils:sys_config_path(State), false};
            Path -> {lists:flatten(io_lib:format("~ts", [Path])), true}
        end,
    Config = consult_config(ConfigFile, Explicit),
    rebar_paths:set_paths([deps, plugins], State),
    case {proplists:get_value(env, Args, dev), proplists:get_value(strict, Args, false)} of
        {Env, Strict} when is_atom(Env), is_boolean(Strict) ->
            Options = #{
                env => Env,
                strict => Strict,
                config => Config,
                config_file => ConfigFile,
                explicit_config => Explicit
            },
            case run(App, Options) of
                ok -> {ok, State};
                {error, Message} -> {error, Message}
            end
    end.

-doc """
Audit and print the report. With `strict => true`, warnings are an error.
`config_file` names where `config` was read from, and `explicit_config`
says whether it was asked for with `--config`.
""".
-spec run(atom(), #{
    env := atom(),
    strict := boolean(),
    config := [{atom(), term()}],
    config_file => string(),
    explicit_config => boolean()
}) ->
    ok | {error, string()}.
run(App, #{env := Env, strict := Strict, config := Config} = Options) ->
    #{warnings := Warnings, infos := Infos, coverage := Coverage} = audit(App, Env, Config),
    ConfigFile = maps:get(config_file, Options, "no config"),
    io:format("~n=== Security Audit ===~n~n  ~ts nova_apps read from ~ts.~n", [Coverage, ConfigFile]),
    case Env =/= dev andalso not maps:get(explicit_config, Options, false) of
        true ->
            io:format(
                "~n  Note: --env ~p reads nova_apps from ~ts; pass --config with the ~p "
                "config if its nova_apps differ.~n",
                [Env, ConfigFile, Env]
            );
        false ->
            ok
    end,
    print_section("WARNINGS", Warnings),
    print_section("INFO", Infos),
    io:format("~n  Summary: ~b warning(s), ~b info(s)~n", [length(Warnings), length(Infos)]),
    case Warnings of
        [_ | _] when Strict -> {error, "Security audit found warnings (--strict)"};
        _ -> ok
    end.

print_section(_Title, []) ->
    ok;
print_section(Title, Lines) ->
    io:format("~n  ~s:~n", [Title]),
    lists:foreach(fun(Line) -> io:format("    ~s~n", [Line]) end, Lines).

-doc """
Compile the routes of `App` and the `nova_apps` declared for it in `Config`
for the Nova environment `Env`, and classify them.
""".
-spec audit(atom(), atom(), [{atom(), term()}]) ->
    #{warnings := [iodata()], infos := [iodata()], coverage := iodata()}.
audit(App, Env, Config) ->
    Apps = nova_apps([App], Config),
    Settings =
        [{nova, environment, Env}] ++
            [
                {Name, router_module, Router}
             || {Name, AppEnv} <- Config,
                is_list(AppEnv),
                {router_module, Router} <- AppEnv
            ],
    Previous = [{Name, Key, application:get_env(Name, Key)} || {Name, Key, _} <- Settings],
    _ = [application:set_env(Name, Key, Value) || {Name, Key, Value} <- Settings],
    Dispatch =
        try
            rebar3_nova_utils:compile_app_routes(Apps)
        after
            lists:foreach(fun restore_env/1, lists:reverse(Previous))
        end,
    ok = rebar3_nova_dispatch:warn_unknown(Dispatch),
    {Warnings, Infos} = classify_findings(collect_routes(Dispatch)),
    #{
        warnings => Warnings,
        infos => Infos,
        coverage => coverage(Env, Apps, Dispatch, Config)
    }.

restore_env({Name, Key, undefined}) -> application:unset_env(Name, Key);
restore_env({Name, Key, {ok, Value}}) -> application:set_env(Name, Key, Value).

coverage(Env, Apps, Dispatch, Config) ->
    Handlers = [
        rebar3_nova_dispatch:handler(P)
     || {_, _, P} <- rebar3_nova_dispatch:routes(Dispatch)
    ],
    Cowboy = length([H || {cowboy, _} = H <- Handlers]),
    NotCovered =
        [io_lib:format("~b websocket/cowboy handler route(s)", [Cowboy]) || Cowboy > 0] ++
            ["static files and error pages"] ++
            [
                "arizona_nova routes (served before nova_router)"
             || lists:keymember(arizona_nova, 1, Config)
            ],
    io_lib:format("Audited routes(~p) of ~ts. Not covered: ~ts.", [
        Env,
        lists:join(", ", [atom_to_list(app_name(A)) || A <- Apps]),
        lists:join(", ", NotCovered)
    ]).

-doc """
`Apps` and, depth first, the `nova_apps` each declares in `Config` (or in
its application env), as Nova resolves them.
""".
-spec nova_apps([atom() | {atom(), map()}], [{atom(), term()}]) -> [atom() | {atom(), map()}].
nova_apps(Apps, Config) ->
    {Resolved, _Seen} = nova_apps(Apps, Config, []),
    Resolved.

nova_apps([], _Config, Seen) ->
    {[], Seen};
nova_apps([Entry | Rest], Config, Seen) ->
    Name = app_name(Entry),
    case lists:member(Name, Seen) of
        true ->
            nova_apps(Rest, Config, Seen);
        false ->
            {Nested, Seen1} = nova_apps(declared_nova_apps(Name, Config), Config, [Name | Seen]),
            {Others, Seen2} = nova_apps(Rest, Config, Seen1),
            {[Entry | Nested] ++ Others, Seen2}
    end.

declared_nova_apps(Name, Config) ->
    case lists:keyfind(Name, 1, Config) of
        {Name, Env} when is_list(Env) -> proplists:get_value(nova_apps, Env, []);
        _ -> application:get_env(Name, nova_apps, [])
    end.

app_name({App, _Options}) -> App;
app_name(App) -> App.

-doc """
The application envs in the sys.config at `Path`. `${VAR}` and
`${VAR:-Default}` are replaced first, from the environment or the default,
as relx does for a `.config.src`. A missing file is an error when it was
asked for and an empty config otherwise.
""".
-spec consult_config(file:filename_all(), boolean()) -> [{atom(), term()}].
consult_config(Path, Explicit) ->
    case file:read_file(Path) of
        {ok, Content} ->
            case parse_config(substitute_env(Content)) of
                {ok, Config} when is_list(Config) ->
                    [{App, Env} || {App, Env} <- Config, is_atom(App)];
                _ ->
                    rebar_api:abort("Could not parse ~ts as a sys.config", [Path])
            end;
        {error, Reason} when Explicit ->
            rebar_api:abort("Could not read ~ts: ~p", [Path, Reason]);
        {error, _} ->
            rebar_api:warn("Could not read config from ~ts", [Path]),
            []
    end.

substitute_env(Content) ->
    case binary:split(Content, ~"${") of
        [Before, Rest] ->
            case binary:split(Rest, ~"}") of
                [Variable, After] ->
                    <<Before/binary, (env_value(Variable))/binary, (substitute_env(After))/binary>>;
                [_] ->
                    Content
            end;
        [_] ->
            Content
    end.

env_value(Variable) ->
    {Name, Default} =
        case binary:split(Variable, ~":-") of
            [Name0, Default0] -> {Name0, Default0};
            [Name0] -> {Name0, <<>>}
        end,
    case os:getenv(binary_to_list(Name)) of
        false ->
            Default;
        Value ->
            case unicode:characters_to_binary(Value) of
                Binary when is_binary(Binary) -> Binary
            end
    end.

parse_config(Content) ->
    case unicode:characters_to_list(Content) of
        Text when is_list(Text) -> parse_term(Text);
        _ -> error
    end.

parse_term(Text) ->
    case erl_scan:string(Text) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> {ok, Term};
                {error, _} -> error
            end;
        _ ->
            error
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

classify_findings(Routes) ->
    Sorted = lists:sort(fun route_order/2, Routes),
    Wildcards = lists:usort([{Path, Module} || {Path, _M, _S, Module, true} <- Sorted]),
    Warnings =
        [
            io_lib:format("~s ~s (~s) has no security", [method_label(Method), Path, Module])
         || {Path, Method, false, Module, _Any} <- Sorted,
            is_mutation(Method)
        ] ++
            [
                io_lib:format("Wildcard method on ~s (~s) - all HTTP methods accepted", [
                    Path, Module
                ])
             || {Path, Module} <- Wildcards
            ],
    Infos = [
        io_lib:format("GET ~s (~s) has no security", [Path, Module])
     || {Path, ~"get", false, Module, _Any} <- Sorted
    ],
    {Warnings, Infos}.

route_order({PathA, MethodA, _, _, _}, {PathB, MethodB, _, _, _}) ->
    {PathA, MethodA} =< {PathB, MethodB}.

method_label(Method) ->
    string:uppercase(binary_to_list(Method)).

is_mutation(~"post") -> true;
is_mutation(~"put") -> true;
is_mutation(~"delete") -> true;
is_mutation(~"patch") -> true;
is_mutation(_) -> false.

collect_routes(Dispatch) ->
    lists:flatmap(fun classify_route/1, rebar3_nova_dispatch:routes(Dispatch)).

classify_route({Path, Method, #nova_handler_value{secure = Secure} = Value}) ->
    case rebar3_nova_dispatch:handler(Value) of
        {controller, Module, _Function} ->
            expand_methods(Method, rebar3_nova_dispatch:openapi_path(Path), Module, Secure);
        {anonymous, Module} ->
            expand_methods(Method, rebar3_nova_dispatch:openapi_path(Path), Module, Secure);
        _StaticOrErrorPage ->
            []
    end;
classify_route({_Path, _Method, _CowboyHandler}) ->
    [].

expand_methods('_', Path, Module, Secure) ->
    Methods = [~"get", ~"post", ~"put", ~"delete", ~"patch"],
    [{Path, M, Secure, Module, true} || M <- Methods];
expand_methods(Method, Path, Module, Secure) ->
    [{Path, rebar3_nova_dispatch:method_to_binary(Method), Secure, Module, false}].
