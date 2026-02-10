-module(rebar3_nova_gen_test).

-export([init/1, do/1, format_error/1]).

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
            {short_desc, "Generate a Common Test suite for a controller"},
            {desc, "Generates a CT suite with test cases for CRUD actions"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(name, Args) of
        undefined ->
            rebar_api:abort("--name is required", []);
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
    SuiteName = io_lib:format("~s_~s_controller_SUITE", [AppName, Name]),
    FileName = filename:join([AppDir, "test", lists:flatten(SuiteName) ++ ".erl"]),
    Content = generate_content(lists:flatten(SuiteName), AppName, Name),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_content(SuiteName, AppName, Name) ->
    iolist_to_binary(io_lib:format(
        "-module(~s).~n"
        "-include_lib(\"common_test/include/ct.hrl\").~n~n"
        "-export([all/0, init_per_suite/1, end_per_suite/1]).~n"
        "-export([test_list/1, test_show/1, test_create/1, test_update/1, test_delete/1]).~n~n"
        "all() ->~n"
        "    [test_list, test_show, test_create, test_update, test_delete].~n~n"
        "init_per_suite(Config) ->~n"
        "    application:ensure_all_started(inets),~n"
        "    application:ensure_all_started(~s),~n"
        "    Config.~n~n"
        "end_per_suite(_Config) ->~n"
        "    application:stop(~s),~n"
        "    ok.~n~n"
        "test_list(_Config) ->~n"
        "    {ok, {{_, 200, _}, _, _}} = httpc:request(\"http://localhost:8080/~s\").~n~n"
        "test_show(_Config) ->~n"
        "    {ok, {{_, 200, _}, _, _}} = httpc:request(\"http://localhost:8080/~s/1\").~n~n"
        "test_create(_Config) ->~n"
        "    {ok, {{_, 201, _}, _, _}} = httpc:request(post, {\"http://localhost:8080/~s\", [], \"application/json\", \"{}\"}, [], []).~n~n"
        "test_update(_Config) ->~n"
        "    {ok, {{_, 200, _}, _, _}} = httpc:request(put, {\"http://localhost:8080/~s/1\", [], \"application/json\", \"{}\"}, [], []).~n~n"
        "test_delete(_Config) ->~n"
        "    {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {\"http://localhost:8080/~s/1\", []}, [], []).~n",
        [SuiteName, AppName, AppName, Name, Name, Name, Name, Name])).
