-module(rebar3_nova_gen_router).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen_router).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {namespace, nova},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 nova gen_router --name api_v1 --prefix /api/v1"},
            {opts, [
                {name, $n, "name", string, "Router name (required)"},
                {prefix, $p, "prefix", {string, ""}, "URL prefix for routes"}
            ]},
            {short_desc, "Generate a Nova router module"},
            {desc, "Generates a router module implementing the nova_router behaviour"}
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
            Prefix = proplists:get_value(prefix, Args, ""),
            generate(AppName, AppDir, Name, Prefix),
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

generate(AppName, AppDir, Name, Prefix) ->
    ModName = io_lib:format("~s_~s_router", [AppName, Name]),
    FileName = filename:join([AppDir, "src", lists:flatten(ModName) ++ ".erl"]),
    Content = generate_content(lists:flatten(ModName), Prefix),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

generate_content(ModName, Prefix) ->
    iolist_to_binary(io_lib:format(
        "-module(~s).~n"
        "-behaviour(nova_router).~n~n"
        "-export([routes/0, prefix/0]).~n~n"
        "prefix() ->~n"
        "    \"~s\".~n~n"
        "routes() ->~n"
        "    [].~n",
        [ModName, Prefix])).
