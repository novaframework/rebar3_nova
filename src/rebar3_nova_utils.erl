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
    default_json_lib/0,
    default_json_lib/1,
    default_environment/0,
    settings_module/0,
    checked_settings/0,
    sys_config_path/1,
    compile_routes/2,
    compile_app_routes/1,
    route_table/1,
    invalid_setting_message/2,
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

-doc "The sys.config `rebar3 shell` uses, `config/dev_sys.config` by default.".
-spec sys_config_path(rebar_state:t()) -> string().
sys_config_path(State) ->
    ShellOpts = rebar_state:get(State, shell, []),
    case proplists:get_value(config, ShellOpts) of
        undefined -> "config/dev_sys.config";
        Path -> lists:flatten(io_lib:format("~ts", [Path]))
    end.

-spec load_sys_config(rebar_state:t()) -> [{atom(), term()}].
load_sys_config(State) ->
    ConfigPath = sys_config_path(State),
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
in. Aborts when that nova predates `nova_routing_trie`, or refuses a
setting of the routes.
""".
-spec compile_app_routes([atom() | {atom(), map()}]) -> nova_routing_trie:trie().
compile_app_routes(Apps) ->
    case route_table(Apps) of
        {ok, Dispatch} ->
            Dispatch;
        {error, {invalid_setting, Setting, Reason}} ->
            rebar_api:abort("~ts", [invalid_setting_message(Setting, Reason)])
    end.

-doc """
As `compile_app_routes/1`, with a setting the nova on the code path refuses
returned and not aborted on: a later Nova than 0.18 checks the settings of a
route group and of the `nova` environment when it compiles routes.
""".
-spec route_table([atom() | {atom(), map()}]) ->
    {ok, nova_routing_trie:trie()} | {error, {invalid_setting, atom(), term()}}.
route_table(Apps) ->
    _ = [application:load(app_name(App)) || App <- Apps],
    _ = code:ensure_loaded(nova_routing_trie),
    case erlang:function_exported(nova_routing_trie, routes, 1) of
        true ->
            Key = {nova_dispatch, rebar3_nova, erlang:unique_integer([positive])},
            try
                {ok, nova_router:compile(Apps, Key)}
            catch
                throw:{error, {invalid_setting, Setting, Reason}} when is_atom(Setting) ->
                    {error, {invalid_setting, Setting, Reason}};
                error:{invalid_setting, Setting, Reason} when is_atom(Setting) ->
                    {error, {invalid_setting, Setting, Reason}}
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

-doc "What to print for a setting Nova refused while it compiled routes.".
-spec invalid_setting_message(atom(), term()) -> io_lib:chars().
invalid_setting_message(Setting, {replaced_by, Replacement}) ->
    io_lib:format(
        "Nova does not compile the routes: ~p is no longer a setting, use ~p",
        [Setting, Replacement]
    );
invalid_setting_message(Setting, {requires, Other}) ->
    io_lib:format("Nova does not compile the routes: ~p needs ~p", [Setting, Other]);
invalid_setting_message(Setting, {required_by, Other}) ->
    io_lib:format("Nova does not compile the routes: ~p needs ~p", [Other, Setting]);
invalid_setting_message(Setting, Value) ->
    io_lib:format("Nova does not compile the routes: ~p cannot be ~p", [Setting, Value]).

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

-doc """
The JSON library the project's Nova uses when `json_lib` is not set.
""".
-spec default_json_lib() -> thoas | json.
default_json_lib() ->
    _ = application:load(nova),
    case application:get_key(nova, applications) of
        {ok, Applications} -> default_json_lib(Applications);
        undefined -> json
    end.

-doc """
`thoas` for a Nova that lists it among its applications (0.18 and earlier),
OTP's `json` for one that does not.
""".
-spec default_json_lib([atom()]) -> thoas | json.
default_json_lib(NovaApplications) ->
    case lists:member(thoas, NovaApplications) of
        true -> thoas;
        false -> json
    end.

-doc """
The environment the project's Nova runs in when `environment` is not set:
`dev` up to Nova 0.18, `prod` for a later one.
""".
-spec default_environment() -> atom().
default_environment() ->
    Configured = application:get_env(nova, environment),
    ok = application:unset_env(nova, environment),
    try nova:get_environment() of
        Default when is_atom(Default) -> Default
    after
        case Configured of
            {ok, Environment} -> application:set_env(nova, environment, Environment);
            undefined -> ok
        end
    end.

-doc """
`nova_settings` when the project's Nova has it, which is a Nova that checks
the settings of `checked_settings/0` when it starts. `undefined` for Nova
0.18 and earlier, which read none of them.
""".
-spec settings_module() -> module() | undefined.
settings_module() ->
    _ = code:ensure_loaded(nova_settings),
    case
        erlang:function_exported(nova_settings, checked, 2) andalso
            erlang:function_exported(nova_settings, expected, 1) andalso
            erlang:function_exported(nova_settings, removed, 0)
    of
        true -> nova_settings;
        false -> undefined
    end.

-doc "The settings a Nova with `settings_module/0` checks at boot, with their defaults.".
-spec checked_settings() -> [{atom(), term()}].
checked_settings() ->
    [
        {non_canonical_paths, reject},
        {trusted_origins, same_host},
        {csrf_token, undefined},
        {secret_key_base, undefined},
        {max_sessions, 1000000},
        {plugins_before_security, []},
        {server_grace, 10000}
    ].
