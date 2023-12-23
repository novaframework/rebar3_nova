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
         OutDir = rebar_app_info:ebin_dir(AppInfo),
         ModelOpts = proplists:unfold(rebar_opts:get(Opts, nova_models, [])),
	 
	 filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
	 rebar_base_compiler:run(Opts,
				 [],
				 option(model_dir, ModelOpts),
				 option(source_ext, ModelOpts),
				 OutDir,
				 option(module_ext, ".beam"),
				 fun(S, T, _C) ->
					 compile_models(S, T, OutDir, ModelOpts, Opts)
				 end,
				 [{check_last_mod, false},
				  {recursive, option(recursive, ModelOpts)}])
     end || AppInfo <- Apps],
    {ok, State}.

option(Opt, Opts) ->
    proplists:get_value(Opt, Opts, default(Opt)).

default(app) -> undefined;
default(model_dir) -> "src/models";
default(source_ext) -> ".erl";
default(module_ext) -> ".beam";
default(out_dir) -> "ebin";
default(compiler_options) -> [verbose, return_errors];
default(recursive) -> true.


compile_models(Source, Target, OutDir, ModelOpts, Opts) ->
    case needs_compile(Source, Target) of
        true ->
            do_compile(Source, Target, OutDir, ModelOpts, Opts);
        false ->
            skipped
    end.

do_compile(Source, Target, OutDir, ModelOpts, Opts) ->
    CompilerOptions = option(compiler_options, ModelOpts),
    LocalOutDir = option(out_dir, ModelOpts),
    OutDir0 = filename:join([OutDir, LocalOutDir]),
    Sorted = proplists:unfold(
               lists:sort(
                 [{out_dir, OutDir0},
                  {model_dir, option(model_dir, ModelOpts)},
                  {compiler_options, CompilerOptions}])),

    %% ensure that doc_root and out_dir are defined,
    %% using defaults if necessary
    OptsU = lists:ukeymerge(1, ModelOpts, Sorted),

    rebar_api:debug("Compiling \"~s\" -> \"~s\" with options:~n    ~s",
                    [Source, Target, io_lib:format("~p", [OptsU])]),
    case boss_record_compiler:compile(Source, OptsU) of
        {ok, Mod} ->
            ok;
        Err ->
            rebar_base_compiler:error_tuple(Source, [], [], OptsU)
    end.

needs_compile(Source, Target) ->
    LM = filelib:last_modified(Target),
    LM < filelib:last_modified(Source).
