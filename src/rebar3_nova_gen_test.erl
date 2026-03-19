-module(rebar3_nova_gen_test).

-export([init/1, do/1, run/1, run/2, format_error/1]).

-define(PROVIDER, gen_test).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova gen_test --name users"},
        {opts, [
            {name, $n, "name", string, "Resource name (required)"}
        ]},
        {short_desc, "Generate a Common Test suite (deprecated: use `rebar3 nova gen test`)"},
        {desc, "Generates a CT suite with test cases for CRUD actions"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:warn("gen_test is deprecated. Use `rebar3 nova gen test` instead.", []),
    run(State).

-spec run(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
run(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    run(State, Args).

-spec run(rebar_state:t(), proplists:proplist()) -> {ok, rebar_state:t()} | {error, string()}.
run(State, Args) ->
    case proplists:get_value(name, Args) of
        undefined ->
            rebar_api:abort("Name is required. Usage: rebar3 nova gen test <name>", []);
        Name ->
            AppName = rebar3_nova_utils:get_app_name(State),
            AppDir = rebar3_nova_utils:get_app_dir(State),
            generate(AppName, AppDir, Name),
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

generate(AppName, AppDir, Name) ->
    SuiteName = lists:flatten(io_lib:format("~s_~s_controller_SUITE", [AppName, Name])),
    FileName = filename:join([AppDir, "test", SuiteName ++ ".erl"]),
    Context = #{
        suite => SuiteName,
        app => atom_to_list(AppName),
        name => Name
    },
    Content = rebar3_nova_utils:render_template(["gen", "test_suite.erl.mustache"], Context),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).
