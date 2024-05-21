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
         flush/0
        ]).

-define(PROVIDER, serve).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},        % The 'user friendly' name of the task
                                 {module, ?MODULE},        % The module implementation of the task
                                 {bare, true},             % The task can be run by the user, always true
                                 {deps, ?DEPS},            % The list of dependencies
                                 {example, "rebar3 nova serve"}, % How to use the plugin
                                 {opts, [{config, undefined, "config", string,
                                          "Path to the config file to use. Defaults to "
                                          "{shell, [{config, File}]} and then the relx "
                                          "sys.config file if not specified."},
                                         {name, undefined, "name", atom,
                                          "Gives a long name to the node."},
                                         {sname, undefined, "sname", atom,
                                          "Gives a short name to the node."},
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
                                          "relx apps if not specified."}]},
                                 {short_desc, "Automatically run compile task on change of source file and reload modules."},
                                 {desc, ""},
                                 {namespace, nova}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec format_error(any()) ->  iolist().
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

-define(VALID_EXTENSIONS,[<<"^(.*)?\.erl$">>,
                          <<"^(.*)?\.dtl$">>]).

auto() ->
    receive
        {ChangedFile, _Events} ->
            Ext = filename:extension(unicode:characters_to_binary(ChangedFile)),
            IsValid = lists:any(
                        fun(ValidExt) ->
                                Result = re:run(Ext, ValidExt),
                                case Result of
                                    {match, _Captured} -> true;
                                    match -> true;
                                    nomatch -> false;
                                    {error, _ErrType} -> false
                                end
                        end,
                        ?VALID_EXTENSIONS),
            case IsValid of
                false -> pass;
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
    after
        0 -> ok
    end.

listen_on_project_apps(State) ->
    CheckoutDeps = [AppInfo ||
                       AppInfo <-rebar_state:all_deps(State),
                       rebar_app_info:is_checkout(AppInfo) == true
                   ],
    ProjectApps = rebar_state:project_apps(State),
    lists:foreach(
      fun(AppInfo) ->
              SrcDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
              ViewsDir = filename:join(SrcDir, "views"),
              CtrlDir = filename:join(SrcDir, "controllers"),
              PrivDir = filename:join(rebar_app_info:dir(AppInfo), "priv"),
              CSrcDir = filename:join(rebar_app_info:dir(AppInfo), "c_src"),
              lists:foreach(fun(Dir) ->
                                    case filelib:is_dir(Dir) of
                                        true -> enotify:start_link(Dir);
                                        false -> ignore
                                    end
                            end, [SrcDir, ViewsDir, CtrlDir, PrivDir, CSrcDir])
      end,
      ProjectApps ++ CheckoutDeps
     ).

remove_from_plugin_paths(State) ->
    PluginPaths = rebar_state:code_paths(State, all_plugin_deps),
    PluginsMinusAuto = lists:filter(
                         fun(Path) ->
                                 Name = filename:basename(Path, "/ebin"),
                                 not (list_to_atom(Name) =:= rebar_auto_plugin
                                      orelse list_to_atom(Name) =:= enotify)
                         end,
                         PluginPaths
                        ),
    rebar_state:code_paths(State, all_plugin_deps, PluginsMinusAuto).


compile_file(<<".erl">>, Filename) ->
    ErlOpts = [],
    case is_routefile(Filename) of
        true ->
            [AppFile|_] = filelib:wildcard(filename:dirname(Filename) ++ "/../src/*.app.src"),
            {ok, [{application, Application, _}|_]} = file:consult(AppFile),
            rebar_api:info("Reloading routefile ~p", [Application]),
            nova_router:process_routefile(Application);
        false ->
            case compile:file(Filename, [binary|ErlOpts]) of
                {ok, ModuleName, Binary} when is_binary(Binary) ->
                    rebar_api:info("Compiled ~p", [ModuleName]),
                    {module, _Mod} = code:load_binary(ModuleName, Filename, Binary),
                    code:purge(ModuleName);
                {ok,ModuleName,Binary,Warnings} ->
                    rebar_api:warn("Compiled ~p with warnings: ~p", [ModuleName, Warnings]),
                    {module, _Mod} = code:load_binary(ModuleName, Filename, Binary),
                    code:purge(ModuleName);
                {error,Errors,Warnings} ->
                    rebar_api:error("Could not compile ~p. Exited with errors: ~p~nWarnings: ~p", [Filename, Errors, Warnings]),
                    ok;
                _ ->
                    rebar_api:error("Could not compile ~p.", [Filename]),
                    ok
            end
    end;

compile_file(<<".dtl">>, Filename) ->


    case erlang:module_loaded(erlydtl) of
        true ->
            %% Continue with the compilation
            Basename = filename:basename(filename:rootname(Filename)++"_dtl"),
            Modname = erlang:list_to_atom(Basename),
            ErlyDTLOpts = [binary],
            case erlydtl:compile_file(Filename, Modname, ErlyDTLOpts ) of
                {ok, ModuleName, undefined} ->
                    rebar_api:info("Compiled ~p", [ModuleName]),
                    code:purge(ModuleName);
                {ok, ModuleName, Binary} ->
                    rebar_api:info("Compiled ~p", [ModuleName]),
                    {module, _Mod} = code:load_binary(ModuleName, Filename, Binary),
                    code:purge(ModuleName);
                {ok,ModuleName,Binary,Warnings} ->
                    rebar_api:warn("Compiled ~p with warnings: ~p", [ModuleName, Warnings]),
                    {module, _Mod} = code:load_binary(ModuleName, Filename, Binary),
                    code:purge(ModuleName);
                {error,Errors,Warnings} ->
                    rebar_api:error("Could not compile ~p. Exited with errors: ~p~nWarnings: ~p", [Filename, Errors, Warnings]),
                    ok;
                _ ->
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
is_routefile([_|Tl]) ->
    is_routefile(Tl).
