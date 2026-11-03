-module(rebar3_nova_middleware).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_middleware_SUITE.
-export([format_route/1]).

-define(PROVIDER, middleware).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova middleware"},
        {opts, []},
        {short_desc, "Show middleware/plugin chain for all routes"},
        {desc, "Lists global and per-route-group plugins configured for the Nova application"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    AppName = rebar3_nova_utils:get_app_name(State),
    Config = rebar3_nova_utils:load_sys_config(State),
    NovaConfig = proplists:get_value(nova, Config, []),
    GlobalPlugins = proplists:get_value(plugins, NovaConfig, []),
    Env = proplists:get_value(environment, NovaConfig, dev),

    io:format("~n=== Global Plugins ===~n"),
    print_plugins(GlobalPlugins, "  "),

    _ = application:load(AppName),
    Router = rebar3_nova_utils:router_module(AppName),
    Groups = get_routes(Router, Env),

    io:format("~n=== Route Groups (~s) ===~n", [Router]),
    print_groups(Groups, GlobalPlugins),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

get_routes(Router, Env) ->
    code:ensure_loaded(Router),
    case erlang:function_exported(Router, routes, 1) of
        true -> Router:routes(Env);
        false -> Router:routes()
    end.

print_groups(Groups, GlobalPlugins) ->
    lists:foreach(
        fun(Group) ->
            Prefix = maps:get(prefix, Group, ""),
            Security = maps:get(security, Group, false),
            Routes = maps:get(routes, Group, []),

            PrefixDisplay =
                case Prefix of
                    "" -> "/";
                    P -> P
                end,
            io:format("~n  Group: prefix=~s  security=~p~n", [PrefixDisplay, Security]),

            io:format("  Plugins:~n"),
            case maps:is_key(plugins, Group) of
                true ->
                    case maps:get(plugins, Group) of
                        [] -> io:format("    (none)~n");
                        Plugins -> print_plugins(Plugins, "    ")
                    end;
                false when GlobalPlugins =:= [] ->
                    io:format("    (inherits global: none)~n");
                false ->
                    io:format("    (inherits global)~n"),
                    print_plugins(GlobalPlugins, "    ")
            end,

            io:format("  Routes:~n"),
            lists:foreach(fun(Route) -> io:format("    ~ts~n", [format_route(Route)]) end, Routes)
        end,
        Groups
    ).

-doc "One route from a router's `routes` list, as a line of text.".
-spec format_route(term()) -> unicode:chardata().
format_route({StatusCode, Handler, _Opts}) when is_integer(StatusCode) ->
    io_lib:format("STATUS ~b -> ~ts", [StatusCode, format_handler(Handler)]);
format_route({Path, Module, #{protocol := ws}}) when is_atom(Module) ->
    io_lib:format("WS ~ts -> ~ts (websocket)", [Path, Module]);
format_route({Path, LocalPath, _Opts}) when is_list(LocalPath) ->
    io_lib:format("STATIC ~ts -> ~ts", [Path, LocalPath]);
format_route({Path, LocalPath}) when is_list(LocalPath) ->
    io_lib:format("STATIC ~ts -> ~ts", [Path, LocalPath]);
format_route({Path, Handler, Opts}) when is_map(Opts) ->
    Methods = maps:get(methods, Opts, ['_']),
    io_lib:format("~ts ~ts -> ~ts", [format_methods(Methods), Path, format_handler(Handler)]);
format_route({Path, Handler}) ->
    io_lib:format("ALL ~ts -> ~ts", [Path, format_handler(Handler)]);
format_route(Other) ->
    io_lib:format("~0p", [Other]).

format_handler(Fun) when is_function(Fun) ->
    {module, Mod} = erlang:fun_info(Fun, module),
    {name, Name} = erlang:fun_info(Fun, name),
    case erlang:fun_info(Fun, type) of
        {type, external} -> io_lib:format("~ts:~ts", [Mod, Name]);
        {type, local} -> io_lib:format("fun in ~ts", [Mod])
    end;
format_handler({Mod, Fun}) when is_atom(Mod), is_atom(Fun) ->
    io_lib:format("~ts:~ts", [Mod, Fun]);
format_handler(Other) ->
    io_lib:format("~0p", [Other]).

format_methods(Methods) ->
    lists:join(",", [format_method(M) || M <- Methods]).

format_method('_') -> "ALL";
format_method(Method) when is_atom(Method) -> string:uppercase(atom_to_list(Method));
format_method(Method) when is_binary(Method) -> string:uppercase(Method);
format_method(Method) -> io_lib:format("~0p", [Method]).

print_plugins([], _Indent) ->
    io:format("~s(none)~n", [_Indent]);
print_plugins(Plugins, Indent) ->
    lists:foreach(
        fun
            ({Type, Plugin, Opts}) ->
                io:format("~s~s: ~s ~p~n", [Indent, Type, Plugin, Opts]);
            ({Type, Plugin}) ->
                io:format("~s~s: ~s~n", [Indent, Type, Plugin]);
            (Other) ->
                io:format("~s~p~n", [Indent, Other])
        end,
        Plugins
    ).
