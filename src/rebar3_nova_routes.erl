-module(rebar3_nova_routes).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, routes).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, nova},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 nova routes list"}, % How to use the plugin
            {opts, [{list, undefined, "list", string, "List all routes"}]},
            {short_desc, "Nova route plugin"},
            {desc, "Plugin to handle nova routes"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [Hd|_] = rebar_state:project_apps(State),
    App = erlang:binary_to_atom(rebar_app_info:name(Hd)),
    nova_router:compile([App]),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
