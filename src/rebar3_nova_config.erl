-module(rebar3_nova_config).

-export([init/1, do/1, format_error/1]).

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
    NovaConfig = proplists:get_value(nova, Config, []),

    io:format("~n=== Nova Configuration ===~n~n"),

    Defaults = [
        {bootstrap_application, required},
        {environment, dev},
        {cowboy_configuration, #{port => 8080}},
        {plugins, []},
        {json_lib, thoas},
        {use_stacktrace, false},
        {dispatch_backend, persistent_term}
    ],

    lists:foreach(fun({Key, Default}) ->
        case proplists:get_value(Key, NovaConfig) of
            undefined when Default =:= required ->
                io:format("  ~-25s *** MISSING (required) ***~n", [atom_to_list(Key)]);
            undefined ->
                io:format("  ~-25s ~p (default)~n", [atom_to_list(Key), Default]);
            Value ->
                io:format("  ~-25s ~p~n", [atom_to_list(Key), Value])
        end
    end, Defaults),

    case proplists:get_value(bootstrap_application, NovaConfig) of
        undefined ->
            rebar_api:warn("~nbootstrap_application is not set in Nova config!", []);
        _ ->
            ok
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
