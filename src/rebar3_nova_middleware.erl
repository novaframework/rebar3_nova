-module(rebar3_nova_middleware).

-export([init/1, do/1, format_error/1]).

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

    Router = erlang:list_to_atom(lists:flatten(io_lib:format("~s_router", [AppName]))),
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
    lists:foreach(fun(Group) ->
        Prefix = maps:get(prefix, Group, ""),
        Security = maps:get(security, Group, false),
        Routes = maps:get(routes, Group, []),

        PrefixDisplay = case Prefix of
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
        lists:foreach(fun(Route) -> print_route(Route) end, Routes)
    end, Groups).

print_route({Path, Handler, Opts}) ->
    Methods = maps:get(methods, Opts, ['_']),
    HandlerStr = format_handler(Handler),
    MethodsStr = format_methods(Methods),
    io:format("    ~s ~s -> ~s~n", [MethodsStr, Path, HandlerStr]);
print_route({Path, Handler}) ->
    HandlerStr = format_handler(Handler),
    io:format("    ALL ~s -> ~s~n", [Path, HandlerStr]).

format_handler(Fun) when is_function(Fun) ->
    {module, Mod} = lists:keyfind(module, 1, erlang:fun_info(Fun)),
    {name, Name} = lists:keyfind(name, 1, erlang:fun_info(Fun)),
    io_lib:format("~s:~s", [Mod, Name]);
format_handler({Mod, Fun}) ->
    io_lib:format("~s:~s", [Mod, Fun]).

format_methods(Methods) ->
    string:join([string:uppercase(erlang:atom_to_list(M)) || M <- Methods], ",").

print_plugins([], _Indent) ->
    io:format("~s(none)~n", [_Indent]);
print_plugins(Plugins, Indent) ->
    lists:foreach(fun
        ({Type, Plugin, Opts}) ->
            io:format("~s~s: ~s ~p~n", [Indent, Type, Plugin, Opts]);
        ({Type, Plugin}) ->
            io:format("~s~s: ~s~n", [Indent, Type, Plugin]);
        (Other) ->
            io:format("~s~p~n", [Indent, Other])
    end, Plugins).
