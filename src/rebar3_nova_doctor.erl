-module(rebar3_nova_doctor).

-export([init/1, do/1, format_error/1]).

-include("nova_router.hrl").
-include_lib("routing_tree/include/routing_tree.hrl").

-ifdef(TEST).
-export([
    summarize/1,
    section_status/1,
    parse_csv/1
]).
-endif.

-define(PROVIDER, doctor).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova doctor"},
        {opts, [
            {only, undefined, "only", string,
                "Comma-separated section ids to run (toolchain,project,config,routes,deps,security,build)"},
            {skip, undefined, "skip", string, "Comma-separated section ids to skip"},
            {strict, $s, "strict", boolean, "Exit 1 on warnings as well as errors"},
            {verbose, $v, "verbose", boolean, "Show all findings, including ok-status detail"}
        ]},
        {short_desc, "Diagnose a Nova project's health"},
        {desc,
            "Runs a series of read-only checks against the current project and reports findings.\n"
            "Sections: toolchain, project, config, routes, deps, security, build.\n"
            "Exit code 0 if no errors (1 with --strict if any warnings)."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, _Args} = rebar_state:command_parsed_args(State),
    Only = parse_csv(proplists:get_value(only, Opts)),
    Skip = parse_csv(proplists:get_value(skip, Opts)),
    Strict = proplists:get_value(strict, Opts, false),
    Verbose = proplists:get_value(verbose, Opts, false),

    Sections = [
        {toolchain, "Toolchain", fun check_toolchain/1},
        {project, "Project structure", fun check_project/1},
        {config, "Configuration", fun check_config/1},
        {routes, "Routes", fun check_routes/1},
        {deps, "Dependencies", fun check_deps/1},
        {security, "Security hygiene", fun check_security/1},
        {build, "Build artifacts", fun check_build/1}
    ],

    Selected = select_sections(Sections, Only, Skip),

    io:format("~n=== Nova Doctor ===~n"),
    Results = [run_section(Title, Fun, State, Verbose) || {_Id, Title, Fun} <- Selected],
    case render_summary(Results, Strict) of
        ok -> {ok, State};
        {error, Reason} -> {error, Reason}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%----------------------------------------------------------------------
%% Section dispatch
%%----------------------------------------------------------------------

select_sections(All, [], []) ->
    All;
select_sections(All, Only, Skip) ->
    Filtered =
        case Only of
            [] -> All;
            _ -> [S || {Id, _, _} = S <- All, lists:member(atom_to_list(Id), Only)]
        end,
    [S || {Id, _, _} = S <- Filtered, not lists:member(atom_to_list(Id), Skip)].

run_section(Title, Fun, State, Verbose) ->
    Findings =
        try
            Fun(State)
        catch
            Class:Reason:Stack ->
                [
                    {error, io_lib:format("check crashed: ~p:~p", [Class, Reason]),
                        io_lib:format("~p", [Stack])}
                ]
        end,
    Status = section_status(Findings),
    render_section(Title, Status, Findings, Verbose),
    {Title, Findings}.

%%----------------------------------------------------------------------
%% Checks
%%----------------------------------------------------------------------

check_toolchain(_State) ->
    OtpRelease = erlang:system_info(otp_release),
    OtpMajor = list_to_integer(OtpRelease),
    OtpFinding =
        case OtpMajor >= 25 of
            true ->
                {ok, io_lib:format("OTP ~s", [OtpRelease])};
            false ->
                {warn, io_lib:format("OTP ~s is old; Nova targets OTP 25+", [OtpRelease]),
                    "Bump your .tool-versions or mise.toml"}
        end,
    {ok, RebarVsn} = application:get_key(rebar, vsn),
    RebarFinding = {ok, io_lib:format("rebar3 ~s", [RebarVsn])},
    ToolVersionsFinding =
        case filelib:is_regular(".tool-versions") orelse filelib:is_regular("mise.toml") of
            true ->
                {ok, "version pinning present"};
            false ->
                {warn, "no .tool-versions or mise.toml",
                    "Pin OTP and rebar3 versions so collaborators don't drift"}
        end,
    [OtpFinding, RebarFinding, ToolVersionsFinding].

check_project(State) ->
    AppName = rebar3_nova_utils:get_app_name(State),
    Config = rebar3_nova_utils:load_sys_config(State),
    NovaConfig = proplists:get_value(nova, Config, []),
    Bootstrap = proplists:get_value(bootstrap_application, NovaConfig),
    BootstrapFinding =
        case Bootstrap of
            undefined ->
                {error, "bootstrap_application not set in sys.config",
                    "Add {bootstrap_application, " ++ atom_to_list(AppName) ++
                        "} under {nova, [...]}"};
            AppName ->
                {ok, io_lib:format("bootstrap_application = ~s", [AppName])};
            Other ->
                {warn,
                    io_lib:format("bootstrap_application = ~p (project app is ~s)", [Other, AppName]),
                    "Usually these should match"}
        end,
    Router = list_to_atom(lists:flatten(io_lib:format("~s_router", [AppName]))),
    code:ensure_loaded(Router),
    RouterFinding =
        case erlang:function_exported(Router, routes, 1) of
            true ->
                {ok, io_lib:format("~s:routes/1 exported", [Router])};
            false ->
                case erlang:function_exported(Router, routes, 0) of
                    true ->
                        {warn, io_lib:format("~s exports routes/0, not routes/1", [Router]),
                            "Newer Nova expects routes/1 taking the environment atom"};
                    false ->
                        {error, io_lib:format("~s:routes/1 not found", [Router]),
                            "Did the app compile? Try `rebar3 compile` first"}
                end
        end,
    AppSrcFinding = check_app_src(State, AppName),
    [BootstrapFinding, RouterFinding, AppSrcFinding].

check_app_src(State, AppName) ->
    Dir = rebar3_nova_utils:get_app_dir(State),
    Path = filename:join([Dir, "src", atom_to_list(AppName) ++ ".app.src"]),
    case file:consult(Path) of
        {ok, [{application, _, Props}]} ->
            Apps = proplists:get_value(applications, Props, []),
            case lists:member(nova, Apps) of
                true ->
                    {ok, ".app.src lists nova in applications"};
                false ->
                    {warn, ".app.src does not list nova in `applications`",
                        "Add nova to the applications list so it starts before your app"}
            end;
        {error, enoent} ->
            {warn, io_lib:format("no ~s.app.src found", [AppName]), ""};
        {error, Reason} ->
            {error, io_lib:format(".app.src parse failed: ~p", [Reason]), ""}
    end.

check_config(State) ->
    Config = rebar3_nova_utils:load_sys_config(State),
    NovaConfig = proplists:get_value(nova, Config, []),
    JsonLib = proplists:get_value(json_lib, NovaConfig, thoas),
    JsonFinding =
        case JsonLib of
            json ->
                {ok, "json_lib = json (OTP built-in)"};
            thoas ->
                {warn, "json_lib defaults to thoas",
                    "Set {json_lib, json} in sys.config to use OTP 27+ built-in json module"};
            Other ->
                {warn, io_lib:format("json_lib = ~p", [Other]),
                    "Prefer OTP `json` module on OTP 27+"}
        end,
    Cowboy = proplists:get_value(cowboy_configuration, NovaConfig, #{port => 8080}),
    CowboyFinding =
        case maps:get(port, Cowboy, undefined) of
            undefined ->
                {warn, "cowboy_configuration has no port", "Add #{port => 8080}"};
            Port when is_integer(Port) ->
                {ok, io_lib:format("listening on port ~b", [Port])};
            BadPort ->
                {error, io_lib:format("invalid port: ~p", [BadPort]), ""}
        end,
    SessionFinding =
        case proplists:get_value(session_backend, NovaConfig) of
            undefined ->
                {warn, "session_backend unset (defaults to ETS)",
                    "Fine for dev; pick a durable backend for production"};
            Backend ->
                {ok, io_lib:format("session_backend = ~p", [Backend])}
        end,
    [JsonFinding, CowboyFinding, SessionFinding].

check_routes(State) ->
    AppName = rebar3_nova_utils:get_app_name(State),
    try nova_router:compile([AppName]) of
        Dispatch ->
            Routes = collect_route_handlers(Dispatch),
            HandlerFindings = [check_handler(R) || R <- Routes],
            DupFinding = check_duplicate_routes(Routes),
            CountFinding =
                {ok, io_lib:format("~b routes compiled", [length(Routes)])},
            [CountFinding | HandlerFindings] ++ DupFinding
    catch
        Class:Reason ->
            [
                {error, io_lib:format("nova_router:compile/1 failed: ~p:~p", [Class, Reason]),
                    "Likely a router module error - run `rebar3 compile` and check exports"}
            ]
    end.

collect_route_handlers(#host_tree{hosts = Hosts}) ->
    lists:flatmap(
        fun({_Host, #routing_tree{tree = Tree}}) -> walk_tree(Tree, <<>>) end,
        Hosts
    ).

walk_tree([], _Prefix) ->
    [];
walk_tree(
    [#node{segment = Segment, is_binding = IsBinding, value = Value, children = Children} | Tl],
    Prefix
) ->
    Seg = seg_bin(Segment, IsBinding),
    NewPrefix = <<Prefix/binary, "/", Seg/binary>>,
    [extract_handler(NC, NewPrefix) || NC <- Value] ++
        walk_tree(Children, NewPrefix) ++
        walk_tree(Tl, Prefix).

seg_bin(S, true) when is_binary(S) -> <<"{", S/binary, "}">>;
seg_bin(S, true) when is_list(S) -> <<"{", (list_to_binary(S))/binary, "}">>;
seg_bin(S, _) when is_binary(S) -> S;
seg_bin(S, _) when is_list(S) -> list_to_binary(S);
seg_bin(S, _) when is_integer(S) -> integer_to_binary(S);
seg_bin(_, _) -> <<"_">>.

extract_handler(
    #node_comp{
        comparator = Method,
        value = #nova_handler_value{
            module = undefined,
            function = undefined,
            callback = Cb
        }
    },
    Path
) when
    is_function(Cb)
->
    {module, M} = lists:keyfind(module, 1, erlang:fun_info(Cb)),
    {Path, Method, M, '$callback', captured};
extract_handler(
    #node_comp{
        comparator = Method,
        value = #nova_handler_value{module = Mod, function = Func}
    },
    Path
) when
    Mod =/= undefined
->
    {Path, Method, Mod, Func, 1};
extract_handler(
    #node_comp{
        comparator = Method,
        value = #cowboy_handler_value{handler = Handler}
    },
    Path
) ->
    {Path, Method, Handler, init, 2};
extract_handler(#node_comp{comparator = Method}, Path) ->
    {Path, Method, unknown, unknown, 1}.

check_handler({_Path, _Method, unknown, _, _}) ->
    {ok, "handler resolved by cowboy directly"};
check_handler({Path, Method, Mod, '$callback', captured}) ->
    code:ensure_loaded(Mod),
    {ok, io_lib:format("~s ~s -> ~s (callback fun)", [fmt_method(Method), Path, Mod])};
check_handler({Path, Method, Mod, Func, Arity}) ->
    code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, Func, Arity) of
        true ->
            {ok, io_lib:format("~s ~s -> ~s:~s/~b", [fmt_method(Method), Path, Mod, Func, Arity])};
        false ->
            {error,
                io_lib:format("~s ~s -> ~s:~s/~b not exported", [
                    fmt_method(Method), Path, Mod, Func, Arity
                ]), "Either the function does not exist or the module did not compile"}
    end.

fmt_method(M) when is_binary(M) -> string:uppercase(binary_to_list(M));
fmt_method(M) -> io_lib:format("~p", [M]).

check_duplicate_routes(Routes) ->
    Keys = [{P, M} || {P, M, _, _, _} <- Routes],
    Sorted = lists:sort(Keys),
    Dups = find_dups(Sorted, []),
    [
        {warn, io_lib:format("duplicate route: ~s ~s", [fmt_method(M), P]),
            "Two handlers respond to the same method+path"}
     || {P, M} <- Dups
    ].

find_dups([], Acc) -> lists:reverse(Acc);
find_dups([X, X | Rest], Acc) -> find_dups([X | Rest], [X | Acc]);
find_dups([_ | Rest], Acc) -> find_dups(Rest, Acc).

check_deps(State) ->
    Dir = rebar3_nova_utils:get_app_dir(State),
    LockPath = filename:join(Dir, "rebar.lock"),
    LockFinding =
        case filelib:is_regular(LockPath) of
            true -> {ok, "rebar.lock present"};
            false -> {warn, "rebar.lock not committed", "Commit rebar.lock for reproducible builds"}
        end,
    Profiles = rebar_state:current_profiles(State),
    ProfileFinding = {ok, io_lib:format("active profiles: ~p", [Profiles])},
    [LockFinding, ProfileFinding].

check_security(State) ->
    Dir = rebar3_nova_utils:get_app_dir(State),
    Gitignore = filename:join(Dir, ".gitignore"),
    GitignoreContent =
        case file:read_file(Gitignore) of
            {ok, B} -> B;
            _ -> <<>>
        end,
    Crash = filename:join(Dir, "erl_crash.dump"),
    CrashFinding =
        case filelib:is_regular(Crash) of
            false ->
                {ok, "no erl_crash.dump in project root"};
            true ->
                case binary:match(GitignoreContent, <<"erl_crash.dump">>) of
                    nomatch ->
                        {warn, "erl_crash.dump in project root, not gitignored",
                            "Add `erl_crash.dump` to .gitignore"};
                    _ ->
                        {warn, "erl_crash.dump in project root", "Delete it (already gitignored)"}
                end
        end,
    EnvFinding =
        case filelib:is_regular(filename:join(Dir, ".env")) of
            false ->
                {ok, "no .env in project root"};
            true ->
                case binary:match(GitignoreContent, <<".env">>) of
                    nomatch ->
                        {error, ".env present but not gitignored",
                            "Add `.env` to .gitignore immediately"};
                    _ ->
                        {ok, ".env present and gitignored"}
                end
        end,
    [CrashFinding, EnvFinding].

check_build(State) ->
    Dir = rebar3_nova_utils:get_app_dir(State),
    BuildDir = filename:join(Dir, "_build/default"),
    BuildFinding =
        case filelib:is_dir(BuildDir) of
            true ->
                {ok, "_build/default present"};
            false ->
                {warn, "_build/default missing",
                    "Run `rebar3 compile` before doctor for full route/handler checks"}
        end,
    AppName = rebar3_nova_utils:get_app_name(State),
    EbinDir = filename:join([BuildDir, "lib", atom_to_list(AppName), "ebin"]),
    EbinFinding =
        case filelib:is_dir(EbinDir) of
            true -> {ok, io_lib:format("~s beam files compiled", [AppName])};
            false -> {warn, io_lib:format("~s not compiled to ~s", [AppName, EbinDir]), ""}
        end,
    [BuildFinding, EbinFinding].

%%----------------------------------------------------------------------
%% Rendering
%%----------------------------------------------------------------------

render_section(Title, Status, Findings, Verbose) ->
    Counts = count_levels(Findings),
    StatusSym = status_symbol(Status),
    io:format("~n~ts ~s~s~n", [StatusSym, Title, count_suffix(Counts)]),
    Visible =
        case Verbose of
            true -> Findings;
            false -> [F || F <- Findings, level(F) =/= ok]
        end,
    lists:foreach(fun render_finding/1, Visible),
    case {Verbose, Status, Visible} of
        {false, ok, _} ->
            {Ok, _, _} = Counts,
            io:format("    (~b ok, run with -v for detail)~n", [Ok]);
        _ ->
            ok
    end.

render_finding({Level, Msg}) ->
    io:format("    ~ts ~ts~n", [level_symbol(Level), Msg]);
render_finding({Level, Msg, ""}) ->
    io:format("    ~ts ~ts~n", [level_symbol(Level), Msg]);
render_finding({Level, Msg, Hint}) ->
    io:format("    ~ts ~ts~n", [level_symbol(Level), Msg]),
    io:format("        hint: ~ts~n", [Hint]).

section_status(Findings) ->
    Levels = [level(F) || F <- Findings],
    case lists:member(error, Levels) of
        true ->
            error;
        false ->
            case lists:member(warn, Levels) of
                true -> warn;
                false -> ok
            end
    end.

level({L, _}) -> L;
level({L, _, _}) -> L.

count_levels(Findings) ->
    lists:foldl(
        fun(F, {O, W, E}) ->
            case level(F) of
                ok -> {O + 1, W, E};
                warn -> {O, W + 1, E};
                error -> {O, W, E + 1}
            end
        end,
        {0, 0, 0},
        Findings
    ).

count_suffix({_, 0, 0}) -> "";
count_suffix({_, W, 0}) -> io_lib:format("    ~b warning(s)", [W]);
count_suffix({_, 0, E}) -> io_lib:format("    ~b error(s)", [E]);
count_suffix({_, W, E}) -> io_lib:format("    ~b warning(s), ~b error(s)", [W, E]).

status_symbol(ok) -> unicode:characters_to_binary([16#2705]);
status_symbol(warn) -> unicode:characters_to_binary([16#26A0, 16#FE0F]);
status_symbol(error) -> unicode:characters_to_binary([16#274C]).

level_symbol(ok) -> unicode:characters_to_binary([16#2713]);
level_symbol(warn) -> unicode:characters_to_binary([$!]);
level_symbol(error) -> unicode:characters_to_binary([$x]).

render_summary(Results, Strict) ->
    AllFindings = lists:append([F || {_, F} <- Results]),
    {Ok, Warn, Err} = count_levels(AllFindings),
    io:format("~n--~nSummary: ~b ok, ~b warning(s), ~b error(s)~n", [Ok, Warn, Err]),
    summarize({Ok, Warn, Err, Strict}).

summarize({_Ok, _Warn, Err, _Strict}) when Err > 0 ->
    {error, lists:flatten(io_lib:format("~b doctor error(s)", [Err]))};
summarize({_Ok, Warn, 0, true}) when Warn > 0 ->
    {error, lists:flatten(io_lib:format("~b doctor warning(s) under --strict", [Warn]))};
summarize(_) ->
    %% Cannot return State here because callers do; this branch is only used in tests
    ok.

parse_csv(undefined) -> [];
parse_csv(Str) -> [string:trim(T) || T <- string:tokens(Str, ",")].

%%----------------------------------------------------------------------
%% Adapter for rebar3 return type
%%----------------------------------------------------------------------
