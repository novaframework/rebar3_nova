%% @doc
%% Developer tool for nova.
%% Heavily inspired by rebar3_auto
%%
-module(rebar3_nova_serve).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-export([
    auto/0,
    flush/0,
    watch_dirs/1
]).

-define(PROVIDER, serve).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        % The 'user friendly' name of the task
        {name, ?PROVIDER},
        % The module implementation of the task
        {module, ?MODULE},
        % The task can be run by the user, always true
        {bare, true},
        % The list of dependencies
        {deps, ?DEPS},
        % How to use the plugin
        {example, "rebar3 nova serve"},
        {opts, [
            {config, undefined, "config", string,
                "Path to the config file to use. Defaults to "
                "{shell, [{config, File}]} and then the relx "
                "sys.config file if not specified."},
            {name, undefined, "name", atom, "Gives a long name to the node."},
            {sname, undefined, "sname", atom, "Gives a short name to the node."},
            {setcookie, undefined, "setcookie", atom,
                "Sets the cookie if the node is distributed."},
            {script_file, undefined, "script", string,
                "Path to an escript file to run before "
                "starting the project apps. Defaults to "
                "rebar.config {shell, [{script_file, File}]} "
                "if not specified."},
            {apps, undefined, "apps", string,
                "A list of apps to boot before starting the "
                "shell. (E.g. --apps app1,app2,app3) Defaults "
                "to rebar.config {shell, [{apps, Apps}]} or "
                "relx apps if not specified."}
        ]},
        {short_desc, "Automatically run compile task on change of source file and reload modules."},
        {desc, ""},
        {namespace, nova}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    spawn(fun() ->
        listen_on_project_apps(State),
        ?MODULE:auto()
    end),
    State1 = remove_from_plugin_paths(State),
    rebar_prv_shell:do(State1).

-define(VALID_EXTENSIONS, [
    <<"^(.*)?\.erl$">>,
    <<"^(.*)?\.dtl$">>
]).

auto() ->
    receive
        {_Pid, {fs, file_event}, {ChangedFile, _Events}} ->
            Ext = filename:extension(unicode:characters_to_binary(ChangedFile)),
            IsValid = lists:any(
                fun(ValidExt) ->
                    Result = re:run(Ext, ValidExt),
                    case Result of
                        {match, _Captured} -> true;
                        match -> true;
                        nomatch -> false
                    end
                end,
                ?VALID_EXTENSIONS
            ),
            case IsValid of
                false ->
                    pass;
                true ->
                    % sleep here so messages can bottle up
                    % or we can flush after compile?
                    timer:sleep(200),
                    flush(),
                    %%rebar_agent:do(compile)
                    compile_file(Ext, ChangedFile)
            end
    end,
    ?MODULE:auto().

flush() ->
    receive
        _ ->
            flush()
    after 0 -> ok
    end.

listen_on_project_apps(State) ->
    CheckoutDeps = [
        AppInfo
     || AppInfo <- rebar_state:all_deps(State),
        rebar_app_info:is_checkout(AppInfo) == true
    ],
    ProjectApps = rebar_state:project_apps(State),
    Dirs = [
        Dir
     || AppInfo <- ProjectApps ++ CheckoutDeps,
        Dir <- watch_dirs(rebar_app_info:dir(AppInfo)),
        filelib:is_dir(Dir)
    ],
    lists:foreach(fun watch/1, lists:enumerate(Dirs)).

watch_dirs(AppDir) ->
    SrcDir = filename:join(AppDir, "src"),
    [
        SrcDir,
        filename:join(SrcDir, "views"),
        filename:join(SrcDir, "controllers"),
        filename:join(AppDir, "priv"),
        filename:join(AppDir, "c_src")
    ].

%% Each watched directory needs its own fs backend, and fs registers the
%% event manager under the name we hand it.
watch({N, Dir}) ->
    Name = list_to_atom("nova_fs_" ++ integer_to_list(N)),
    case fs:start_link(Name, Dir) of
        {ok, _Pid} ->
            fs:subscribe(Name);
        {error, Reason} ->
            rebar_api:warn("Not watching ~ts for changes: ~p", [Dir, Reason])
    end.

remove_from_plugin_paths(State) ->
    PluginPaths = rebar_state:code_paths(State, all_plugin_deps),
    PluginsMinusAuto = lists:filter(
        fun(Path) ->
            Name = filename:basename(Path, "/ebin"),
            not (list_to_atom(Name) =:= rebar_auto_plugin orelse
                list_to_atom(Name) =:= fs)
        end,
        PluginPaths
    ),
    rebar_state:code_paths(State, all_plugin_deps, PluginsMinusAuto).

compile_file(<<".erl">>, Filename) ->
    ErlOpts = [],
    case is_routefile(Filename) of
        true ->
            [AppFile | _] = filelib:wildcard(filename:dirname(Filename) ++ "/../src/*.app.src"),
            {ok, [{application, Application, _} | _]} = file:consult(AppFile),
            rebar_api:info("Reloading routefile ~p", [Application]),
            nova_router:compile(Application);
        false ->
            case compile:file(Filename, [binary, return_errors, return_warnings | ErlOpts]) of
                {ok, ModuleName, Binary} ->
                    rebar_api:info("Compiled ~p", [ModuleName]),
                    {module, _Mod} = code:load_binary(ModuleName, Filename, Binary),
                    code:purge(ModuleName);
                {ok, ModuleName, Binary, Warnings} ->
                    rebar_api:warn("Compiled ~p with warnings: ~p", [ModuleName, Warnings]),
                    {module, _Mod} = code:load_binary(ModuleName, Filename, Binary),
                    code:purge(ModuleName);
                {error, Errors, Warnings} ->
                    rebar_api:error("Could not compile ~p. Exited with errors: ~p~nWarnings: ~p", [
                        Filename, Errors, Warnings
                    ]),
                    ok
            end
    end;
compile_file(<<".dtl">>, Filename) ->
    case erlang:module_loaded(erlydtl) of
        true ->
            %% Continue with the compilation
            Basename = filename:basename(filename:rootname(Filename) ++ "_dtl"),
            Modname = erlang:list_to_atom(Basename),
            ErlyDTLOpts = [binary],
            case erlydtl:compile_file(Filename, Modname, ErlyDTLOpts) of
                {ok, ModuleName} ->
                    rebar_api:info("Compiled ~p", [ModuleName]),
                    code:purge(ModuleName);
                {ok, ModuleName, Warnings} ->
                    rebar_api:warn("Compiled ~p with warnings: ~p", [ModuleName, Warnings]),
                    code:purge(ModuleName);
                {error, Errors, Warnings} ->
                    rebar_api:error(
                        "Could not compile ~p. Exited with errors: ~p~nWarnings: ~p",
                        [Filename, Errors, Warnings]
                    ),
                    ok
            end;
        _ ->
            {module, _} = code:load_file(erlydtl),
            compile_file(<<".dtl">>, Filename)
    end.

is_routefile([]) ->
    false;
is_routefile(".routes.erl") ->
    %% Reload routes
    true;
is_routefile([_ | Tl]) ->
    is_routefile(Tl).
