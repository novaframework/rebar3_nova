-module(rebar3_nova_utils).

-export([
    get_app_name/1,
    get_app_dir/1,
    ensure_dir/1,
    write_file/2,
    write_file_if_not_exists/2,
    copy_priv_file/2,
    parse_actions/1,
    load_sys_config/1,
    compile_routes/2,
    compile_app_routes/1,
    router_module/1,
    to_binary/1
]).

-spec get_app_name(rebar_state:t()) -> atom().
get_app_name(State) ->
    [Hd | _] = rebar_state:project_apps(State),
    erlang:binary_to_atom(rebar_app_info:name(Hd)).

-spec get_app_dir(rebar_state:t()) -> file:filename().
get_app_dir(State) ->
    [Hd | _] = rebar_state:project_apps(State),
    rebar_app_info:dir(Hd).

-spec ensure_dir(file:filename_all()) -> ok.
ensure_dir(Path) ->
    filelib:ensure_dir(Path).

-spec write_file(file:filename_all(), iodata()) -> ok.
write_file(Path, Content) ->
    ok = ensure_dir(Path),
    ok = file:write_file(Path, Content),
    log_info("Created ~s", [Path]),
    ok.

-spec write_file_if_not_exists(file:filename_all(), iodata()) -> ok | skipped.
write_file_if_not_exists(Path, Content) ->
    case filelib:is_regular(Path) of
        true ->
            log_warn("File already exists, skipping: ~s", [Path]),
            skipped;
        false ->
            write_file(Path, Content)
    end.

-spec copy_priv_file(file:filename(), file:filename()) -> ok.
copy_priv_file(PrivRelPath, DestPath) ->
    PrivDir = code:priv_dir(rebar3_nova),
    SrcPath = filename:join(PrivDir, PrivRelPath),
    ok = ensure_dir(DestPath),
    {ok, _} = file:copy(SrcPath, DestPath),
    log_info("Created ~s", [DestPath]),
    ok.

-spec parse_actions(string()) -> [atom()].
parse_actions(Str) ->
    Tokens = string:tokens(Str, ","),
    [erlang:list_to_atom(string:trim(T)) || T <- Tokens].

-spec load_sys_config(rebar_state:t()) -> [{atom(), term()}].
load_sys_config(State) ->
    ShellOpts = rebar_state:get(State, shell, []),
    ConfigPath =
        case proplists:get_value(config, ShellOpts) of
            undefined -> "config/dev_sys.config";
            Path -> Path
        end,
    case file:consult(ConfigPath) of
        {ok, [Config]} when is_list(Config) -> Config;
        {ok, Config} when is_list(Config) -> Config;
        {error, _} ->
            rebar_api:warn("Could not read config from ~s", [ConfigPath]),
            []
    end.

-doc """
Compile `App`'s routes with the project's own Nova rather than the copy
bundled with this plugin, so the dispatch table matches what the
application serves. Needs nova 0.18 or later in the project.
""".
-spec compile_routes(rebar_state:t(), atom()) -> nova_routing_trie:trie().
compile_routes(State, App) ->
    rebar_paths:set_paths([deps, plugins], State),
    compile_app_routes([App]).

-doc """
Compile the routes of `Apps`, entries as in `nova_apps`, with the nova on
the code path, into a table of their own so earlier compiles do not merge
in. Aborts when that nova predates `nova_routing_trie`.
""".
-spec compile_app_routes([atom() | {atom(), map()}]) -> nova_routing_trie:trie().
compile_app_routes(Apps) ->
    _ = [application:load(app_name(App)) || App <- Apps],
    _ = code:ensure_loaded(nova_routing_trie),
    case erlang:function_exported(nova_routing_trie, routes, 1) of
        true ->
            Key = {nova_dispatch, rebar3_nova, erlang:unique_integer([positive])},
            try
                nova_router:compile(Apps, Key)
            after
                nova_router:delete_dispatch(Key)
            end;
        false ->
            rebar_api:abort(
                "rebar3_nova reads routes with nova_routing_trie, which needs nova 0.18 or "
                "later. Upgrade the project's nova dependency.",
                []
            )
    end.

app_name({App, _Options}) -> App;
app_name(App) -> App.

-doc "Text as a UTF-8 binary.".
-spec to_binary(unicode:chardata()) -> binary().
to_binary(Chars) ->
    case unicode:characters_to_binary(Chars) of
        Bin when is_binary(Bin) -> Bin
    end.

-doc "The router module Nova uses for `App`: its `router_module` env, or `App_router`.".
-spec router_module(atom()) -> module().
router_module(App) ->
    case application:get_env(App, router_module) of
        {ok, Router} when is_atom(Router) -> Router;
        _ -> list_to_atom(atom_to_list(App) ++ "_router")
    end.

%%----------------------------------------------------------------------
%% Internal: safe logging (rebar_api may not be available in tests)
%%----------------------------------------------------------------------

log_info(Fmt, Args) ->
    try
        rebar_api:info(Fmt, Args)
    catch
        error:undef -> io:format(Fmt ++ "~n", Args)
    end.

log_warn(Fmt, Args) ->
    try
        rebar_api:warn(Fmt, Args)
    catch
        error:undef -> io:format("Warning: " ++ Fmt ++ "~n", Args)
    end.
