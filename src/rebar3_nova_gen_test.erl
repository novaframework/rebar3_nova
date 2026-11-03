-module(rebar3_nova_gen_test).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_gen_SUITE.
-export([controller_routes/2, generate_content/3]).

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
            Controller = list_to_atom(
                lists:flatten(io_lib:format("~s_~s_controller", [AppName, Name]))
            ),
            Dispatch = rebar3_nova_utils:compile_routes(State, AppName),
            Routes =
                case controller_routes(Dispatch, Controller) of
                    [] ->
                        rebar_api:warn(
                            "No routes for ~s found; the suite assumes the gen_resource routes under /~s",
                            [Controller, Name]
                        ),
                        default_routes(Name);
                    Found ->
                        Found
                end,
            generate(AppName, AppDir, Name, Routes),
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-type route() :: {Method :: binary(), Path :: binary(), Function :: atom()}.

-doc "The routes served by `Controller` in a compiled dispatch table, in path order.".
-spec controller_routes(nova_routing_trie:trie(), module()) -> [route()].
controller_routes(Dispatch, Controller) ->
    lists:usort([
        {rebar3_nova_dispatch:method_to_binary(Method), Path, Function}
     || {Path, Method, Payload} <- rebar3_nova_dispatch:routes(Dispatch),
        {controller, Module, Function} <- [rebar3_nova_dispatch:handler(Payload)],
        Module =:= Controller
    ]).

default_routes(Name) ->
    Base = unicode:characters_to_binary("/" ++ Name),
    [
        {~"get", Base, list},
        {~"get", <<Base/binary, "/:id">>, show},
        {~"post", Base, create},
        {~"put", <<Base/binary, "/:id">>, update},
        {~"delete", <<Base/binary, "/:id">>, delete}
    ].

generate(AppName, AppDir, Name, Routes) ->
    SuiteName = lists:flatten(io_lib:format("~s_~s_controller_SUITE", [AppName, Name])),
    FileName = filename:join([AppDir, "test", SuiteName ++ ".erl"]),
    Content = generate_content(SuiteName, AppName, Routes),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

-doc """
A suite with one case per route. It reads the port from the nova
`cowboy_configuration` the tests run with, uses `1` for path bindings and
sends a body matching the gen_resource schema, so it passes against the
generated controller as it is.
""".
-spec generate_content(string(), atom(), [route()]) -> binary().
generate_content(SuiteName, AppName, Routes) ->
    Cases = test_cases(Routes),
    App = atom_to_list(AppName),
    Names = [Case || {Case, _, _, _} <- Cases],
    unicode:characters_to_binary([
        "-module(",
        SuiteName,
        ").\n"
        "\n"
        "-include_lib(\"common_test/include/ct.hrl\").\n"
        "-include_lib(\"stdlib/include/assert.hrl\").\n"
        "\n"
        "-export([all/0, init_per_suite/1, end_per_suite/1]).\n"
        "-export([\n",
        lists:join(",\n", ["    " ++ N ++ "/1" || N <- Names]),
        "\n"
        "]).\n"
        "\n"
        "all() ->\n"
        "    [\n",
        lists:join(",\n", ["        " ++ N || N <- Names]),
        "\n"
        "    ].\n"
        "\n"
        "init_per_suite(Config) ->\n"
        "    {ok, _} = application:ensure_all_started(inets),\n"
        "    {ok, _} = application:ensure_all_started(",
        App,
        "),\n"
        "    Config.\n"
        "\n"
        "end_per_suite(_Config) ->\n"
        "    ok = application:stop(",
        App,
        ").\n",
        [test_case(Case) || Case <- Cases],
        "\n"
        "request(Method, Path) ->\n"
        "    httpc:request(Method, {url(Path), []}, [], []).\n"
        "\n"
        "request(Method, Path, Body) ->\n"
        "    httpc:request(Method, {url(Path), [], \"application/json\", Body}, [], []).\n"
        "\n"
        "url(Path) ->\n"
        "    Cowboy = application:get_env(nova, cowboy_configuration, #{}),\n"
        "    \"http://localhost:\" ++ integer_to_list(maps:get(port, Cowboy, 8080)) ++ Path.\n"
    ]).

test_cases(Routes) ->
    Named = [{"test_" ++ atom_to_list(F), M, P} || {M, P, F} <- Routes],
    Shared = [Name || {Name, _, _} <- Named] -- lists:usort([Name || {Name, _, _} <- Named]),
    [
        {case_name(Name, Method, Shared), Method, request_path(Path), expected_status(Method)}
     || {Name, Method, Path} <- Named
    ].

case_name(Name, Method, Shared) ->
    case lists:member(Name, Shared) of
        true -> Name ++ "_" ++ binary_to_list(Method);
        false -> Name
    end.

request_path(Path) ->
    Segments = [binding_value(S) || S <- binary:split(Path, ~"/", [global])],
    binary_to_list(iolist_to_binary(lists:join(~"/", Segments))).

binding_value(<<":", _/binary>>) -> ~"1";
binding_value(Segment) -> Segment.

expected_status(~"post") -> "201";
expected_status(~"delete") -> "204";
expected_status(_) -> "200".

test_case({Name, Method, Path, Status}) ->
    Request =
        case Method of
            M when M =:= ~"post"; M =:= ~"put"; M =:= ~"patch" ->
                ["request(", M, ", \"", Path, "\", ~\"{\\\"name\\\": \\\"test\\\"}\")"];
            ~"any" ->
                ["request(get, \"", Path, "\")"];
            M ->
                ["request(", M, ", \"", Path, "\")"]
        end,
    [
        "\n",
        Name,
        "(_Config) ->\n"
        "    ?assertMatch({ok, {{_, ",
        Status,
        ", _}, _, _}}, ",
        Request,
        ").\n"
    ].
