-module(rebar3_nova_utils).

-export([get_app_name/1, get_app_dir/1, ensure_dir/1,
         write_file_if_not_exists/2, parse_actions/1, load_sys_config/1]).

-spec get_app_name(rebar_state:t()) -> atom().
get_app_name(State) ->
    [Hd|_] = rebar_state:project_apps(State),
    erlang:binary_to_atom(rebar_app_info:name(Hd)).

-spec get_app_dir(rebar_state:t()) -> file:filename().
get_app_dir(State) ->
    [Hd|_] = rebar_state:project_apps(State),
    rebar_app_info:dir(Hd).

-spec ensure_dir(file:filename()) -> ok.
ensure_dir(Path) ->
    filelib:ensure_dir(Path).

-spec write_file_if_not_exists(file:filename(), iodata()) -> ok | skipped.
write_file_if_not_exists(Path, Content) ->
    case filelib:is_regular(Path) of
        true ->
            rebar_api:warn("File already exists, skipping: ~s", [Path]),
            skipped;
        false ->
            ok = ensure_dir(Path),
            ok = file:write_file(Path, Content),
            rebar_api:info("Created ~s", [Path]),
            ok
    end.

-spec parse_actions(string()) -> [atom()].
parse_actions(Str) ->
    Tokens = string:tokens(Str, ","),
    [erlang:list_to_atom(string:trim(T)) || T <- Tokens].

-spec load_sys_config(rebar_state:t()) -> [{atom(), term()}].
load_sys_config(State) ->
    ShellOpts = rebar_state:get(State, shell, []),
    ConfigPath = case proplists:get_value(config, ShellOpts) of
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
