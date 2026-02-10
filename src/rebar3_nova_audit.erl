-module(rebar3_nova_audit).

-export([init/1, do/1, format_error/1]).

-include("nova_router.hrl").
-include_lib("routing_tree/include/routing_tree.hrl").

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
        [] -> ok;
        _ ->
            io:format("~n  WARNINGS:~n"),
            lists:foreach(fun(W) -> io:format("    ~s~n", [W]) end, Warnings)
    end,

    case Infos of
        [] -> ok;
        _ ->
            io:format("~n  INFO:~n"),
            lists:foreach(fun(I) -> io:format("    ~s~n", [I]) end, Infos)
    end,

    io:format("~n  Summary: ~b warning(s), ~b info(s)~n",
              [length(Warnings), length(Infos)]),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

classify_findings(Routes) ->
    lists:foldl(fun({Path, Method, Secure, Module, IsWildcard}, {WAcc, IAcc}) ->
        MethodStr = string:uppercase(erlang:binary_to_list(Method)),
        W1 = case is_mutation(Method) andalso Secure =:= false of
            true ->
                [io_lib:format("~s ~s (~s) has no security", [MethodStr, Path, Module]) | WAcc];
            false ->
                WAcc
        end,
        W2 = case IsWildcard of
            true ->
                [io_lib:format("Wildcard method on ~s (~s) - all HTTP methods accepted", [Path, Module]) | W1];
            false ->
                W1
        end,
        I1 = case Method =:= <<"get">> andalso Secure =:= false of
            true ->
                [io_lib:format("GET ~s (~s) has no security", [Path, Module]) | IAcc];
            false ->
                IAcc
        end,
        {W2, I1}
    end, {[], []}, Routes).

is_mutation(<<"post">>) -> true;
is_mutation(<<"put">>) -> true;
is_mutation(<<"delete">>) -> true;
is_mutation(<<"patch">>) -> true;
is_mutation(_) -> false.

collect_routes(#host_tree{hosts = Hosts}) ->
    lists:flatmap(fun({_Host, #routing_tree{tree = Tree}}) ->
        collect_nodes(Tree, <<>>)
    end, Hosts).

collect_nodes([], _Prefix) -> [];
collect_nodes([#node{is_wildcard = true}|Tl], Prefix) ->
    collect_nodes(Tl, Prefix);
collect_nodes([#node{segment = Segment}|Tl], Prefix) when is_integer(Segment) ->
    collect_nodes(Tl, Prefix);
collect_nodes([#node{segment = Segment, is_binding = IsBinding, value = Value, children = Children}|Tl], Prefix) ->
    SegBin = segment_to_binary(Segment, IsBinding),
    NewPrefix = <<Prefix/binary, "/", SegBin/binary>>,
    HandlerRoutes = lists:filtermap(fun(NodeComp) -> classify_handler(NodeComp, NewPrefix) end, Value),
    lists:flatten(HandlerRoutes) ++ collect_nodes(Children, NewPrefix) ++ collect_nodes(Tl, Prefix).

segment_to_binary(Segment, true) when is_binary(Segment) ->
    <<"{", Segment/binary, "}">>;
segment_to_binary(Segment, _) when is_binary(Segment) ->
    Segment;
segment_to_binary(Segment, IsBinding) when is_list(Segment) ->
    segment_to_binary(erlang:list_to_binary(Segment), IsBinding).

classify_handler(#node_comp{value = #nova_handler_value{module = nova_file_controller}}, _Path) ->
    false;
classify_handler(#node_comp{value = #nova_handler_value{module = nova_error_controller}}, _Path) ->
    false;
classify_handler(#node_comp{comparator = Method,
                            value = #nova_handler_value{module = undefined, function = undefined,
                                                        callback = Callback, secure = Secure}}, Path) ->
    {module, Module} = lists:keyfind(module, 1, erlang:fun_info(Callback)),
    expand_methods(Method, Path, Module, Secure);
classify_handler(#node_comp{comparator = Method,
                            value = #nova_handler_value{module = Module, secure = Secure}}, Path) ->
    expand_methods(Method, Path, Module, Secure);
classify_handler(#node_comp{value = #cowboy_handler_value{}}, _Path) ->
    false.

expand_methods('_', Path, Module, Secure) ->
    Methods = [<<"get">>, <<"post">>, <<"put">>, <<"delete">>, <<"patch">>],
    {true, [{Path, M, Secure, Module, true} || M <- Methods]};
expand_methods(Method, Path, Module, Secure) ->
    {true, [{Path, method_to_binary(Method), Secure, Module, false}]}.

method_to_binary(Method) when is_atom(Method) ->
    erlang:atom_to_binary(Method);
method_to_binary(Method) when is_binary(Method) ->
    string:lowercase(Method).
