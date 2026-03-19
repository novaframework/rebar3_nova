-module(rebar3_nova_utils).

-export([
    get_app_name/1,
    get_app_dir/1,
    ensure_dir/1,
    write_file/2,
    write_file_if_not_exists/2,
    copy_priv_file/2,
    parse_actions/1,
    load_sys_config/1
]).

-spec get_app_name(rebar_state:t()) -> atom().
get_app_name(State) ->
    [Hd | _] = rebar_state:project_apps(State),
    erlang:binary_to_atom(rebar_app_info:name(Hd)).

-spec get_app_dir(rebar_state:t()) -> file:filename().
get_app_dir(State) ->
    [Hd | _] = rebar_state:project_apps(State),
    rebar_app_info:dir(Hd).

-spec ensure_dir(file:filename()) -> ok.
ensure_dir(Path) ->
    filelib:ensure_dir(Path).

-spec write_file(file:filename(), iodata()) -> ok.
write_file(Path, Content) ->
    ok = ensure_dir(Path),
    ok = file:write_file(Path, Content),
    log_info("Created ~s", [Path]),
    ok.

-spec write_file_if_not_exists(file:filename(), iodata()) -> ok | skipped.
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
