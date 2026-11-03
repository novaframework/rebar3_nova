-module(rebar3_nova_audit).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_dispatch_SUITE.
-export([collect_routes/1, classify_findings/1]).

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
    Dispatch = rebar3_nova_utils:compile_routes(State, AppName),
    ok = rebar3_nova_dispatch:warn_unknown(Dispatch),
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
