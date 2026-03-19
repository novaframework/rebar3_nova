-module(rebar3_nova_utils).

-export([
    get_app_name/1,
    get_app_dir/1,
    ensure_dir/1,
    write_file/2,
    write_file_if_not_exists/2,
    copy_priv_file/2,
    parse_actions/1,
    parse_fields/1,
    load_sys_config/1,
    singularize/1,
    pluralize/1,
    capitalize/1,
    timestamp/0,
    render_template/2
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
    case file:copy(SrcPath, DestPath) of
        {ok, _} ->
            log_info("Created ~s", [DestPath]),
            ok;
        {error, Reason} ->
            rebar_api:abort(
                "Could not copy ~s to ~s: ~p",
                [SrcPath, DestPath, Reason]
            )
    end.

-spec parse_actions(string()) -> [atom()].
parse_actions(Str) ->
    Tokens = string:tokens(Str, ","),
    [erlang:list_to_atom(string:trim(T)) || T <- Tokens].

-spec parse_fields(string()) -> [{string(), string()}].
parse_fields(Str) ->
    Pairs = string:tokens(Str, ","),
    [parse_field(string:trim(P)) || P <- Pairs].

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

-spec singularize(string()) -> string().
singularize(Name) ->
    case lists:reverse(Name) of
        [$s | Rest] -> lists:reverse(Rest);
        _ -> Name
    end.

-spec pluralize(string()) -> string().
pluralize(Name) ->
    case lists:last(Name) of
        $s -> Name;
        _ -> Name ++ "s"
    end.

-spec capitalize(string()) -> string().
capitalize([H | T]) when H >= $a, H =< $z ->
    [H - 32 | T];
capitalize(Other) ->
    Other.

-spec timestamp() -> string().
timestamp() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    lists:flatten(
        io_lib:format(
            "~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
            [Y, Mo, D, H, Mi, S]
        )
    ).

-spec render_template([string()], map()) -> binary().
render_template(TemplatePath, Context) ->
    PrivDir = code:priv_dir(rebar3_nova),
    Path = filename:join([PrivDir, "templates" | TemplatePath]),
    case file:read_file(Path) of
        {ok, Template} ->
            bbmustache:render(Template, Context, [{key_type, atom}]);
        {error, Reason} ->
            rebar_api:abort(
                "Could not read template: ~s~n"
                "Reason: ~p~n"
                "This may indicate a corrupt or incomplete rebar3_nova installation. "
                "Try: rm -rf ~/.cache/rebar3/plugins/rebar3_nova && rebar3 compile",
                [Path, Reason]
            )
    end.

%%----------------------------------------------------------------------
%% Internal
%%----------------------------------------------------------------------

parse_field(Pair) ->
    case string:tokens(Pair, ":") of
        [Name, Type] ->
            validate_field_type(string:trim(Type)),
            {string:trim(Name), string:trim(Type)};
        [Name] ->
            {string:trim(Name), "string"};
        _ ->
            rebar_api:abort(
                "Invalid field spec: '~s'~n"
                "Expected format: name:type (e.g., email:string, age:integer)~n"
                "Supported types: string, integer, float, boolean, date, datetime, text, binary, uuid",
                [Pair]
            )
    end.

validate_field_type(Type) ->
    ValidTypes = ["string", "integer", "float", "boolean", "date", "datetime",
                  "text", "binary", "uuid", "bigint", "decimal", "map", "array"],
    case lists:member(Type, ValidTypes) of
        true -> ok;
        false ->
            rebar_api:warn(
                "Unknown field type '~s'. Supported types: string, integer, float, "
                "boolean, date, datetime, text, binary, uuid, bigint, decimal, map, array",
                [Type]
            )
    end.

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
