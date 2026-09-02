-module(rebar3_nova_audit).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_dispatch_SUITE.
-export([collect_routes/1]).

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
        {example, "rebar3 nova audit"},
        {opts, []},
        {short_desc, "Audit route security configuration"},
        {desc, "Checks routes for security issues like unsecured mutations and wildcard methods"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    AppName = rebar3_nova_utils:get_app_name(State),
    Dispatch = nova_router:compile([AppName]),
    Routes = collect_routes(Dispatch),

    {Warnings, Infos} = classify_findings(Routes),

    io:format("~n=== Security Audit ===~n"),

    case Warnings of
        [] ->
            ok;
        _ ->
            io:format("~n  WARNINGS:~n"),
            lists:foreach(fun(W) -> io:format("    ~s~n", [W]) end, Warnings)
    end,

    case Infos of
        [] ->
            ok;
        _ ->
            io:format("~n  INFO:~n"),
            lists:foreach(fun(I) -> io:format("    ~s~n", [I]) end, Infos)
    end,

    io:format(
        "~n  Summary: ~b warning(s), ~b info(s)~n",
        [length(Warnings), length(Infos)]
    ),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

classify_findings(Routes) ->
    lists:foldl(
        fun({Path, Method, Secure, Module, IsWildcard}, {WAcc, IAcc}) ->
            MethodStr = string:uppercase(erlang:binary_to_list(Method)),
            W1 =
                case is_mutation(Method) andalso Secure =:= false of
                    true ->
                        [
                            io_lib:format("~s ~s (~s) has no security", [MethodStr, Path, Module])
                            | WAcc
                        ];
                    false ->
                        WAcc
                end,
            W2 =
                case IsWildcard of
                    true ->
                        [
                            io_lib:format(
                                "Wildcard method on ~s (~s) - all HTTP methods accepted", [
                                    Path, Module
                                ]
                            )
                            | W1
                        ];
                    false ->
                        W1
                end,
            I1 =
                case Method =:= <<"get">> andalso Secure =:= false of
                    true ->
                        [io_lib:format("GET ~s (~s) has no security", [Path, Module]) | IAcc];
                    false ->
                        IAcc
                end,
            {W2, I1}
        end,
        {[], []},
        Routes
    ).

is_mutation(<<"post">>) -> true;
is_mutation(<<"put">>) -> true;
is_mutation(<<"delete">>) -> true;
is_mutation(<<"patch">>) -> true;
is_mutation(_) -> false.

collect_routes(Dispatch) ->
    lists:flatmap(fun classify_route/1, rebar3_nova_dispatch:routes(Dispatch)).

classify_route({_Path, _Method, #nova_handler_value{module = nova_file_controller}}) ->
    [];
classify_route({_Path, _Method, #nova_handler_value{module = nova_error_controller}}) ->
    [];
classify_route(
    {Path, Method, #nova_handler_value{
        module = undefined,
        function = undefined,
        callback = Callback,
        secure = Secure
    }}
) ->
    {module, Module} = lists:keyfind(module, 1, erlang:fun_info(Callback)),
    expand_methods(Method, rebar3_nova_dispatch:openapi_path(Path), Module, Secure);
classify_route({Path, Method, #nova_handler_value{module = Module, secure = Secure}}) ->
    expand_methods(Method, rebar3_nova_dispatch:openapi_path(Path), Module, Secure);
classify_route({_Path, _Method, #cowboy_handler_value{}}) ->
    [].

expand_methods('_', Path, Module, Secure) ->
    Methods = [<<"get">>, <<"post">>, <<"put">>, <<"delete">>, <<"patch">>],
    [{Path, M, Secure, Module, true} || M <- Methods];
expand_methods(Method, Path, Module, Secure) ->
    [{Path, rebar3_nova_dispatch:method_to_binary(Method), Secure, Module, false}].
