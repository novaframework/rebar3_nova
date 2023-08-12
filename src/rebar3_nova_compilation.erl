-module(rebar3_nova_compilation).
-behaviour(provider).

-export([
         init/1,
         do/1,
         format_error/1
        ]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, false},
                                 {deps, ?DEPS},
                                 {example, "rebar3 nova compile"},
                                 {opts, []},
                                 {namespace, nova}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.


format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         Dir = rebar_app_info:dir(AppInfo),
         OutDir = rebar_app_info:ebin_dir(AppInfo),

         NovaModelOpts = proplists:unfold(rebar_opts:get(Opts, nova_models, [])),
         lists:foreach(fun(NovaModelOpts0) ->
                               ModelDir = filename:join(Dir, option(model_dir, NovaModelOpts0)),
                               NovaModelOpts1 = [{model_dir, ModelDir}|proplists:delete(model_dir, NovaModelOpts0)],
                               filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),

                               rebar_base_compiler:run(Opts,
                                                       [],
                                                       ModelDir,
                                                       option(source_ext, NovaModelOpts1),
                                                       OutDir,
                                                       option(module_ext, NovaModelOpts1) ++ ".beam",
                                                       fun(S, T, C) ->
                                                               compile_models(C, S, T, NovaModelOpts1, Dir, OutDir)
                                                       end,
                                                       [{check_last_mod, false},
                                                        {recursive, option(recursive, NovaModelOpts1)}])
                       end, expand_opts(NovaModelOpts))
     end || AppInfo <- Apps],
    {ok, State}.

expand_opts(Opts) ->
    SharedOpts = lists:filter(fun(X) -> is_tuple(X) end, Opts),
    OptsLists  = case lists:filter(fun(X) -> is_list(X) end, Opts) of
                   [] -> [[]];
                   L  -> L
                 end,
    lists:map(fun(X) -> lists:ukeymerge(1, proplists:unfold(X), SharedOpts) end, OptsLists).

option(Opt, DtlOpts) ->
    proplists:get_value(Opt, DtlOpts, default(Opt)).

default(app) -> undefined;
default(model_dir) -> "src/models";
default(source_ext) -> ".erl";
default(module_ext) -> "_model";
default(compiler_options) -> [debug_info, return];
default(recursive) -> true.


compile_models(_, Source, Target, DtlOpts, Dir, OutDir) ->
    case needs_compile(Source, Target, DtlOpts) of
        true ->
            do_compile(Source, Target, DtlOpts, Dir, OutDir);
        false ->
            skipped
    end.

do_compile(Source, Target, ModelOpts, Dir, OutDir) ->
    CompilerOptions = option(compiler_options, ModelOpts),

    Sorted = proplists:unfold(
               lists:sort(
                 [{out_dir, OutDir},
                  {model_dir, filename:join(Dir, option(model_dir, ModelOpts))},
                  {compiler_options, CompilerOptions}])),

    %% ensure that doc_root and out_dir are defined,
    %% using defaults if necessary
    Opts = lists:ukeymerge(1, ModelOpts, Sorted),
    rebar_api:debug("Compiling \"~s\" -> \"~s\" with options:~n    ~s",
                    [Source, Target, io_lib:format("~p", [Opts])]),
    case boss_record_compiler:compile(Source, ModelOpts) of
        {ok, _Mod} ->
            ok;
        _ ->
            rebar_base_compiler:error_tuple(Source, [], [], Opts)
    end.

needs_compile(Source, Target, DtlOpts) ->
    LM = filelib:last_modified(Target),
    LM < filelib:last_modified(Source).
