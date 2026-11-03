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
    watch_dirs/1,
    fs_backend/1,
    watcher_problem/2,
    reload_routes/1
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

auto() ->
    receive
        {_Pid, {fs, file_event}, {ChangedFile, _Events}} ->
            Ext = filename:extension(unicode:characters_to_binary(ChangedFile)),
            case lists:member(Ext, [~".erl", ~".dtl"]) of
                false ->
                    pass;
                true ->
                    timer:sleep(200),
                    flush(),
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
    OsType = os:type(),
    case watcher_problem(OsType, watcher_found(fs_backend(OsType))) of
        none ->
            lists:foreach(fun watch/1, lists:enumerate(Dirs));
        Problem ->
            rebar_api:warn("~ts Code reloading is off; restart serve after installing it.", [
                Problem
            ])
    end.

-doc "The fs backend for an OS, mirroring the choice fs_sup makes.".
-spec fs_backend({atom(), atom()}) -> module() | undefined.
fs_backend({unix, darwin}) -> fsevents;
fs_backend({unix, linux}) -> inotifywait;
fs_backend({unix, freebsd}) -> inotifywait;
fs_backend({unix, openbsd}) -> inotifywait;
fs_backend({unix, sunos}) -> undefined;
fs_backend({unix, _}) -> kqueue;
fs_backend({win32, nt}) -> inotifywait_win32;
fs_backend(_) -> undefined.

watcher_found(undefined) -> unsupported;
watcher_found(Backend) -> Backend:find_executable() =/= false.

-doc "Why file watching cannot start, or `none` when it can.".
-spec watcher_problem({atom(), atom()}, boolean() | unsupported) -> none | string().
watcher_problem(_OsType, true) ->
    none;
watcher_problem(_OsType, unsupported) ->
    "rebar3 nova serve cannot watch files on this OS.";
watcher_problem({unix, linux}, false) ->
    "rebar3 nova serve needs inotifywait to watch files. Install inotify-tools "
    "(apt install inotify-tools, dnf install inotify-tools, pacman -S inotify-tools).";
watcher_problem({unix, _}, false) ->
    "rebar3 nova serve needs inotifywait to watch files. Install inotify-tools from ports or "
    "packages.";
watcher_problem(_OsType, false) ->
    "rebar3 nova serve could not find the fs file watcher executable.".

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
    case compile:file(Filename, [binary, return_errors, return_warnings | ErlOpts]) of
        {ok, ModuleName, Binary} ->
            rebar_api:info("Compiled ~p", [ModuleName]),
            load(ModuleName, Filename, Binary);
        {ok, ModuleName, Binary, Warnings} ->
            rebar_api:warn("Compiled ~p with warnings: ~p", [ModuleName, Warnings]),
            load(ModuleName, Filename, Binary);
        {error, Errors, Warnings} ->
            rebar_api:error("Could not compile ~p. Exited with errors: ~p~nWarnings: ~p", [
                Filename, Errors, Warnings
            ]),
            ok
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

load(ModuleName, Filename, Binary) ->
    {module, _Mod} = code:load_binary(ModuleName, Filename, Binary),
    code:purge(ModuleName),
    reload_routes(ModuleName).

-doc """
Re-register the routes of every compiled application whose router is
`Module`, so an edited router takes effect without a restart. An application
from `nova_apps` keeps the options of its entry there. Only the default
dispatch table is refreshed: applications on listeners of their own are not.
""".
-spec reload_routes(module()) -> [atom()].
reload_routes(Module) ->
    Apps = [
        {App, Prefix}
     || {App, Prefix} <- compiled_apps(),
        rebar3_nova_utils:router_module(App) =:= Module
    ],
    [reload_app(App, Prefix) || {App, Prefix} <- Apps].

compiled_apps() ->
    try
        nova_router:compiled_apps()
    catch
        error:_ -> []
    end.

reload_app(App, Prefix) ->
    ok = nova_router:remove_application(App),
    _ = nova_router:compile([{App, app_options(App, Prefix)}]),
    rebar_api:info("Reloaded routes for ~p", [App]),
    App.

%% An app listed in nova_apps is compiled with the options of its entry there
%% (prefix, secure, plugins, plugin_strategy, ...), found the way Nova
%% resolves nested nova_apps: depth first from the bootstrap application.
app_options(App, Prefix) ->
    case application:get_env(nova, bootstrap_application) of
        {ok, Bootstrap} when is_atom(Bootstrap), Bootstrap =/= App ->
            case declared_options(App, nova_apps(Bootstrap), [Bootstrap]) of
                {found, Options} -> Options;
                {not_found, _Seen} -> prefix_options(Prefix)
            end;
        _ ->
            prefix_options(Prefix)
    end.

declared_options(_App, [], Seen) ->
    {not_found, Seen};
declared_options(App, [Entry | Rest], Seen) ->
    case nova_app_entry(Entry) of
        {App, Options} ->
            {found, Options};
        {Name, _Options} ->
            case lists:member(Name, Seen) of
                true ->
                    declared_options(App, Rest, Seen);
                false ->
                    case declared_options(App, nova_apps(Name), [Name | Seen]) of
                        {found, _} = Found -> Found;
                        {not_found, Seen1} -> declared_options(App, Rest, Seen1)
                    end
            end
    end.

nova_app_entry({Name, Options}) when is_atom(Name), is_map(Options) -> {Name, Options};
nova_app_entry(Name) when is_atom(Name) -> {Name, #{}}.

nova_apps(App) ->
    case application:get_env(App, nova_apps, []) of
        Apps when is_list(Apps) -> Apps;
        _ -> []
    end.

prefix_options("/") -> #{};
prefix_options("") -> #{};
prefix_options(Prefix) -> #{prefix => Prefix}.
