-module(rebar3_nova_routes).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_dispatch_SUITE.
-export([print_routes/1]).

-include("nova_router.hrl").

-define(PROVIDER, routes).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        % The 'user friendly' name of the task
        {name, ?PROVIDER},
        % The module implementation of the task
        {module, ?MODULE},
        {namespace, nova},
        % The task can be run by the user, always true
        {bare, true},
        % The list of dependencies
        {deps, ?DEPS},
        % How to use the plugin
        {example, "rebar3 nova routes list"},
        {opts, [{list, undefined, "list", string, "List all routes"}]},
        {short_desc, "Nova route plugin"},
        {desc, "Plugin to handle nova routes"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [Hd | _] = rebar_state:project_apps(State),
    App = erlang:binary_to_atom(rebar_app_info:name(Hd)),
    Dispatch = nova_router:compile([App]),
    print_routes(Dispatch),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private functions
%% ===================================================================
print_routes(Dispatch) ->
    Routes = lists:sort(nova_routing_trie:routes(Dispatch)),
    [print_route(Route) || Route <- Routes],
    ok.

print_route({Host, Path, Method, Payload}) ->
    io:format(
        "~-8ts ~-40ts ~ts~ts~n",
        [
            rebar3_nova_dispatch:method_to_binary(Method),
            format_path(Path),
            format_handler(Payload),
            format_host(Host)
        ]
    ).

format_path(Path) when is_integer(Path) ->
    %% A status-code route rather than a URL.
    io_lib:format("(status ~b)", [Path]);
format_path(Path) ->
    Path.

format_host('_') -> <<>>;
format_host(Host) -> io_lib:format("  [host ~ts]", [Host]).

format_handler(#nova_handler_value{module = undefined, function = undefined, callback = Callback}) when
    is_function(Callback)
->
    {module, Module} = lists:keyfind(module, 1, erlang:fun_info(Callback)),
    {name, Function} = lists:keyfind(name, 1, erlang:fun_info(Callback)),
    io_lib:format("~ts:~ts/1", [Module, Function]);
format_handler(#nova_handler_value{module = Module, function = Function}) ->
    io_lib:format("~ts:~ts/1", [Module, Function]);
format_handler(#cowboy_handler_value{handler = Handler}) ->
    io_lib:format("~ts (cowboy handler)", [Handler]);
format_handler(Other) ->
    io_lib:format("~p", [Other]).
