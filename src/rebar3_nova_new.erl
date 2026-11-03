-module(rebar3_nova_new).

-export([init/1, do/1, format_error/1]).
-export([generate_project/2, validate_flags/1]).

-define(PROVIDER, new).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example,
            "rebar3 nova new myapp [--kura] [--pgo] [--arizona] [--lfe] [--ci] [--docker] [--otel]"},
        {opts, [
            {name, $n, "name", string, "Project name"},
            {kura, undefined, "kura", boolean, "Include Kura database layer"},
            {pgo, undefined, "pgo", boolean, "Include PGO PostgreSQL client"},
            {arizona, undefined, "arizona", boolean, "Include Arizona live views"},
            {lfe, undefined, "lfe", boolean, "Generate LFE source files"},
            {ci, undefined, "ci", boolean, "Generate GitHub Actions CI workflow"},
            {docker, undefined, "docker", boolean, "Generate Dockerfile"},
            {otel, undefined, "otel", boolean, "Include OpenTelemetry instrumentation"}
        ]},
        {short_desc, "Create a new Nova project"},
        {desc, "Create a new Nova project with composable flags"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, Args} = rebar_state:command_parsed_args(State),
    Name = resolve_name(Opts, Args),
    Flags = parse_flags(Opts),
    case validate_flags(Flags) of
        ok ->
            generate_project(Name, Flags),
            print_summary(Name, Flags),
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%======================================================================
%% Flag parsing
%%======================================================================

resolve_name(Opts, Args) ->
    case proplists:get_value(name, Opts) of
        undefined ->
            case Args of
                [N | _] -> N;
                _ -> error(missing_project_name)
            end;
        N ->
            N
    end.

parse_flags(Opts) ->
    #{
        kura => proplists:get_value(kura, Opts, false),
        pgo => proplists:get_value(pgo, Opts, false),
        arizona => proplists:get_value(arizona, Opts, false),
        lfe => proplists:get_value(lfe, Opts, false),
        ci => proplists:get_value(ci, Opts, false),
        docker => proplists:get_value(docker, Opts, false),
        otel => proplists:get_value(otel, Opts, false)
    }.

validate_flags(#{kura := true, pgo := true}) ->
    {error,
        "--kura and --pgo are mutually exclusive: kura_postgres already brings its own "
        "PostgreSQL driver"};
validate_flags(_) ->
    ok.

%%======================================================================
%% Project generation
%%======================================================================

generate_project(Name, Flags) ->
    case filelib:is_dir(Name) of
        true ->
            error({dir_exists, Name});
        false ->
            ok
    end,
    generate_rebar_config(Name, Flags),
    generate_app_src(Name, Flags),
    generate_app(Name, Flags),
    generate_sup(Name, Flags),
    generate_router(Name, Flags),
    generate_controller(Name, Flags),
    generate_dev_sys_config(Name, Flags),
    generate_prod_sys_config(Name, Flags),
    generate_vm_args(Name),
    generate_tool_versions(Name),
    generate_gitignore(Name),
    copy_favicon(Name),
    maybe_generate_view(Name, Flags),
    maybe_generate_kura(Name, Flags),
    maybe_generate_pgo(Name, Flags),
    maybe_generate_arizona(Name, Flags),
    maybe_generate_ci(Name, Flags),
    maybe_generate_docker(Name, Flags).

%%======================================================================
%% rebar.config
%%======================================================================

generate_rebar_config(Name, Flags) ->
    Path = filename:join(Name, "rebar.config"),
    Content = [
        "%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-\n\n",
        "{erl_opts, [debug_info]}.\n",
        "{src_dirs, [{\"src\", [{recursive, true}]}]}.\n",
        "{shell, [{config, \"./config/dev_sys.config.src\"}]}.\n\n",
        rebar_erlydtl_opts(Flags),
        rebar_deps(Flags),
        rebar_relx(Name, Flags),
        rebar_profiles(Flags),
        "{dialyzer, [{plt_apps, all_deps}]}.\n\n",
        rebar_plugins(Flags),
        rebar_erlfmt(Flags),
        rebar_provider_hooks(Flags),
        rebar_xref()
    ],
    rebar3_nova_utils:write_file(Path, Content).

rebar_erlydtl_opts(#{arizona := true}) ->
    [];
rebar_erlydtl_opts(#{lfe := true}) ->
    [
        "{erlydtl_opts, [\n",
        "    {doc_root, \"src/views\"},\n",
        "    {recursive, true},\n",
        "    {libraries, [\n",
        "        {nova_erlydtl_inventory, nova_erlydtl_inventory}\n",
        "    ]},\n",
        "    {default_libraries, [nova_erlydtl_inventory]}\n",
        "]}.\n\n"
    ];
rebar_erlydtl_opts(_) ->
    [
        "{erlydtl_opts, [\n",
        "    {doc_root, \"src/views\"},\n",
        "    {recursive, true},\n",
        "    {libraries, [\n",
        "        {nova_erlydtl_inventory, nova_erlydtl_inventory}\n",
        "    ]},\n",
        "    {default_libraries, [nova_erlydtl_inventory]}\n",
        "]}.\n\n"
    ].

rebar_deps(Flags) ->
    BaseDeps =
        case maps:get(lfe, Flags) of
            true -> ["    {nova, \"~> 0.18\"},\n", "    {logjam, \"1.2.4\"}"];
            false -> ["    {nova, \"~> 0.18\"},\n", "    {flatlog, \"0.1.2\"}"]
        end,
    KuraDep =
        case maps:get(kura, Flags) of
            true -> [",\n    {kura, \"~> 2.25\"},\n    {kura_postgres, \"~> 1.1\"}"];
            false -> []
        end,
    PgoDep =
        case maps:get(pgo, Flags) of
            true -> [",\n    pgo"];
            false -> []
        end,
    ArizonaDeps =
        case maps:get(arizona, Flags) of
            true -> [",\n    {arizona_nova, \"~> 0.1\"}"];
            false -> []
        end,
    LfeDep =
        case maps:get(lfe, Flags) of
            true ->
                [",\n    {lfe, {git, \"http://github.com/rvirding/lfe\", {branch, \"develop\"}}}"];
            false ->
                []
        end,
    OtelDeps =
        case maps:get(otel, Flags) of
            true ->
                [
                    ",\n    {opentelemetry, \"~> 1.7\"},\n    {opentelemetry_api, \"~> 1.5\"},\n"
                    "    {opentelemetry_exporter, \"~> 1.11\"},\n    {opentelemetry_nova, \"~> 0.2\"}"
                ];
            false ->
                []
        end,
    ["{deps, [\n", BaseDeps, KuraDep, PgoDep, ArizonaDeps, LfeDep, OtelDeps, "\n]}.\n\n"].

rebar_relx(Name, _Flags) ->
    [
        "{relx, [\n",
        "    {release, {",
        Name,
        ", git}, [\n",
        "        ",
        Name,
        ",\n",
        "        sasl\n",
        "    ]},\n",
        "    {mode, dev},\n",
        "    {sys_config_src, \"./config/dev_sys.config.src\"},\n",
        "    {vm_args_src, \"./config/vm.args.src\"}\n",
        "]}.\n\n"
    ].

rebar_profiles(_Flags) ->
    [
        "{profiles, [\n",
        "    {prod, [\n",
        "        {relx, [\n",
        "            {mode, prod},\n",
        "            {sys_config_src, \"./config/prod_sys.config.src\"}\n",
        "        ]}\n",
        "    ]},\n",
        "    {test, [\n",
        "        {erl_opts, [nowarn_export_all, warnings_as_errors, {i, \"test/util\"}]},\n",
        "        {deps, [\n",
        "            {meck, \"1.2.0\"}\n",
        "        ]},\n",
        "        {ct_opts, [{sys_config, \"./config/dev_sys.config.src\"}]},\n",
        "        {extra_src_dirs, [{\"test\", [{recursive, true}]}]},\n",
        "        {xref_extra_paths, [\"test\"]}\n",
        "    ]}\n",
        "]}.\n\n"
    ].

rebar_plugins(Flags) ->
    ErlydtlPlugin =
        case maps:get(arizona, Flags) of
            true ->
                [];
            false ->
                [
                    "    {rebar3_erlydtl_plugin, \".*\",\n",
                    "        {git, \"https://github.com/erlydtl/rebar3_erlydtl_plugin.git\",\n",
                    "            {ref, \"9d520e4775b12c796185f65b40f17773a3ff212b\"}}},\n"
                ]
        end,
    LfePlugin =
        case maps:get(lfe, Flags) of
            true ->
                [
                    "    {rebar3_lfe, {git, \"http://github.com/lfe-rebar3/rebar3_lfe\", {branch, \"release/0.4.x\"}}},\n"
                ];
            false ->
                []
        end,
    KuraPlugin =
        case maps:get(kura, Flags) of
            true -> ["    {rebar3_kura, \"~> 0.16\"},\n"];
            false -> []
        end,
    [
        "{project_plugins, [\n",
        ErlydtlPlugin,
        LfePlugin,
        "    {rebar3_nova, \"~> 0.13\"},\n",
        KuraPlugin,
        "    {erlfmt, \"~>1.7\"}\n",
        "]}.\n\n"
    ].

rebar_erlfmt(Flags) ->
    Migrations =
        case maps:get(kura, Flags) of
            true -> ", \"src/migrations/*.erl\"";
            false -> ""
        end,
    [
        "{erlfmt, [\n",
        "    write,\n",
        "    {files, [\n",
        "        \"rebar.config\",\n",
        "        \"src/*.app.src\",\n",
        "        \"src/**/{*.erl, *.hrl}\",\n",
        "        \"test/**/{*.erl, *.hrl}\"\n",
        "    ]},\n",
        "    {exclude_files, [\"config/vm.args.src\", \"config/prod_sys.config.src\"",
        Migrations,
        "]}\n",
        "]}.\n\n"
    ].

rebar_provider_hooks(Flags) ->
    Hooks =
        ["{compile, {erlydtl, compile}}" || not maps:get(arizona, Flags)] ++
            ["{compile, {lfe, compile}}" || maps:get(lfe, Flags)] ++
            ["{compile, {kura, compile}}" || maps:get(kura, Flags)],
    PreHooks =
        case Hooks of
            [Hook] -> ["[", Hook, "]"];
            _ -> ["[\n        ", lists:join(",\n        ", Hooks), "\n    ]"]
        end,
    case Hooks of
        [] ->
            [];
        _ ->
            [
                "%% Run the code generators before the Erlang sources compile\n",
                "{provider_hooks, [\n",
                "    {pre, ",
                PreHooks,
                "}\n",
                "]}.\n\n"
            ]
    end.

rebar_xref() ->
    [
        "{xref_checks, [\n",
        "    undefined_function_calls,\n",
        "    undefined_functions,\n",
        "    locals_not_used,\n",
        "    deprecated_function_calls,\n",
        "    deprecated_functions\n",
        "]}.\n"
    ].

%%======================================================================
%% app.src
%%======================================================================

generate_app_src(Name, Flags) ->
    Path = filename:join([Name, "src", Name ++ ".app.src"]),
    Apps = app_src_applications(Flags),
    Content = [
        "%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-\n",
        "{application, ",
        Name,
        ", [\n",
        "    {description, \"",
        Name,
        " managed by Nova\"},\n",
        "    {vsn, git},\n",
        "    {registered, []},\n",
        "    {mod, {",
        Name,
        "_app, []}},\n",
        "    {included_applications, []},\n",
        "    {applications, [\n",
        "        ",
        Apps,
        "\n",
        "    ]},\n",
        "    {env, []},\n",
        "    {modules, []},\n",
        "    {maintainers, []},\n",
        "    {licenses, [\"Apache 2.0\"]},\n",
        "    {links, []}\n",
        "]}.\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

app_src_applications(Flags) ->
    Base = ["kernel,\n        stdlib,\n        nova"],
    Kura =
        case maps:get(kura, Flags) of
            true -> [",\n        kura,\n        kura_postgres"];
            false -> []
        end,
    Pgo =
        case maps:get(pgo, Flags) of
            true -> [",\n        pgo"];
            false -> []
        end,
    Arizona =
        case maps:get(arizona, Flags) of
            true -> [",\n        arizona_nova"];
            false -> []
        end,
    Otel =
        case maps:get(otel, Flags) of
            true ->
                [
                    ",\n        opentelemetry_exporter,\n        opentelemetry,\n        opentelemetry_nova"
                ];
            false ->
                []
        end,
    [Base, Kura, Pgo, Arizona, Otel].

%%======================================================================
%% app.erl / app.lfe
%%======================================================================

generate_app(Name, #{lfe := true}) ->
    Path = filename:join([Name, "src", Name ++ "_app.lfe"]),
    Content = [
        "(defmodule ",
        Name,
        "_app\n",
        "  (behaviour gen_server)\n",
        "  (export\n",
        "    ;; app implementation\n",
        "    (start 2)\n",
        "    (stop 0)))\n\n",
        "(include-lib \"logjam/include/logjam.hrl\")\n\n",
        "(defun start (_type _args)\n",
        "  (log-info \"Starting ",
        Name,
        " application ...\")\n",
        "  (",
        Name,
        ".sup:start_link))\n\n",
        "(defun stop ()\n",
        "  (",
        Name,
        ".sup:stop)\n",
        "  'ok)\n"
    ],
    rebar3_nova_utils:write_file(Path, Content);
generate_app(Name, Flags) ->
    Path = filename:join([Name, "src", Name ++ "_app.erl"]),
    Otel =
        case maps:get(otel, Flags) of
            true -> ["    ok = opentelemetry_nova:setup(),\n"];
            false -> []
        end,
    Migrate =
        case maps:get(kura, Flags) of
            true -> ["    {ok, _} = kura_migrator:migrate(", Name, "_repo),\n"];
            false -> []
        end,
    Content = [
        "-module(",
        Name,
        "_app).\n\n",
        "-behaviour(application).\n\n",
        "-export([start/2, stop/1]).\n\n",
        "start(_StartType, _StartArgs) ->\n",
        Otel,
        Migrate,
        "    ",
        Name,
        "_sup:start_link().\n\n",
        "stop(_State) ->\n",
        "    ok.\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% sup.erl / sup.lfe
%%======================================================================

generate_sup(Name, #{lfe := true}) ->
    Path = filename:join([Name, "src", Name ++ ".sup.lfe"]),
    Content = [
        "(defmodule ",
        Name,
        ".sup\n",
        "  (behaviour supervisor)\n",
        "  (export\n",
        "   (start_link 0)\n",
        "   (stop 0)\n",
        "   (init 1)))\n\n",
        "(defun SERVER () (MODULE))\n",
        "(defun supervisor-opts () '())\n",
        "(defun sup-flags ()\n",
        "  `#M(strategy one_for_all\n",
        "      intensity 0\n",
        "      period 1))\n\n",
        "(defun start_link ()\n",
        "  (supervisor:start_link `#(local ,(SERVER))\n",
        "                         (MODULE)\n",
        "                         (supervisor-opts)))\n\n",
        "(defun stop ()\n",
        "  (gen_server:call (SERVER) 'stop))\n\n",
        "(defun init (_args)\n",
        "  `#(ok #(,(sup-flags) ())))\n"
    ],
    rebar3_nova_utils:write_file(Path, Content);
generate_sup(Name, _Flags) ->
    Path = filename:join([Name, "src", Name ++ "_sup.erl"]),
    Content = [
        "-module(",
        Name,
        "_sup).\n\n",
        "-behaviour(supervisor).\n\n",
        "-export([start_link/0]).\n",
        "-export([init/1]).\n\n",
        "-define(SERVER, ?MODULE).\n\n",
        "start_link() ->\n",
        "    supervisor:start_link({local, ?SERVER}, ?MODULE, []).\n\n",
        "init([]) ->\n",
        "    SupFlags = #{strategy => one_for_all},\n",
        "    ChildSpecs = [],\n",
        "    {ok, {SupFlags, ChildSpecs}}.\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% router
%%======================================================================

generate_router(Name, #{lfe := true, arizona := true}) ->
    Path = filename:join([Name, "src", Name ++ "_router.lfe"]),
    Content = [
        "(defmodule ",
        Name,
        "_router\n",
        "  (export\n",
        "   (routes 1)))\n\n",
        "(defun routes\n",
        "  (_)\n",
        "   `(#M(prefix \"\"\n",
        "         security false\n",
        "         routes\n",
        "           (\n",
        "            #(\"/heartbeat\" ,(lambda (_) #(status 200)) #M(methods (get)))\n",
        "            #(\"/assets/[...]\" \"assets\")\n",
        "            ))))\n"
    ],
    rebar3_nova_utils:write_file(Path, Content);
generate_router(Name, #{lfe := true}) ->
    Path = filename:join([Name, "src", Name ++ "_router.lfe"]),
    Content = [
        "(defmodule ",
        Name,
        "_router\n",
        "  (export\n",
        "   (routes 1)))\n\n",
        "(defun routes\n",
        "  (_)\n",
        "   `(#M(prefix \"\"\n",
        "         security false\n",
        "         routes\n",
        "           (\n",
        "            #(\"/heartbeat\" ,(lambda (_) #(status 200)) #M(methods (get)))\n",
        "            #(\"/\" ,(lambda (params) (",
        Name,
        "_main_controller:index params))\n",
        "              #M(methods (get)))\n",
        "            ))))\n"
    ],
    rebar3_nova_utils:write_file(Path, Content);
generate_router(Name, #{arizona := true}) ->
    Path = filename:join([Name, "src", Name ++ "_router.erl"]),
    Content = [
        "-module(",
        Name,
        "_router).\n",
        "-behaviour(nova_router).\n\n",
        "-export([routes/1]).\n\n",
        "%% Live pages are declared in the arizona_nova env in config/*_sys.config.src.\n",
        "routes(_Environment) ->\n",
        "    [\n",
        "        #{\n",
        "            prefix => \"\",\n",
        "            security => false,\n",
        "            routes => [\n",
        "                {\"/heartbeat\", fun(_) -> {status, 200} end, #{methods => [get]}},\n",
        "                {\"/assets/[...]\", \"assets\"}\n",
        "            ]\n",
        "        }\n",
        "    ].\n"
    ],
    rebar3_nova_utils:write_file(Path, Content);
generate_router(Name, _Flags) ->
    Path = filename:join([Name, "src", Name ++ "_router.erl"]),
    Content = [
        "-module(",
        Name,
        "_router).\n",
        "-behaviour(nova_router).\n\n",
        "-export([routes/1]).\n\n",
        "routes(_Environment) ->\n",
        "    [\n",
        "        #{\n",
        "            prefix => \"\",\n",
        "            security => false,\n",
        "            routes => [\n",
        "                {\"/\", fun ",
        Name,
        "_main_controller:index/1, #{methods => [get]}},\n",
        "                {\"/heartbeat\", fun(_) -> {status, 200} end, #{methods => [get]}}\n",
        "            ]\n",
        "        }\n",
        "    ].\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% controller
%%======================================================================

generate_controller(_Name, #{arizona := true}) ->
    ok;
generate_controller(Name, #{lfe := true}) ->
    Path = filename:join([Name, "src", "controllers", Name ++ "_main_controller.lfe"]),
    Content = [
        "(defmodule ",
        Name,
        "_main_controller\n",
        "  (export\n",
        "   (index 1)))\n\n",
        "(defun index\n",
        "  (_)\n",
        "    `#(status 200 #M() \"nova is running!\"))\n"
    ],
    rebar3_nova_utils:write_file(Path, Content);
generate_controller(Name, _Flags) ->
    Path = filename:join([Name, "src", "controllers", Name ++ "_main_controller.erl"]),
    Content = [
        "-module(",
        Name,
        "_main_controller).\n\n",
        "-export([index/1]).\n\n",
        "index(_Req) ->\n",
        "    {ok, [{message, \"Hello world!\"}]}.\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% dev_sys.config.src
%%======================================================================

generate_dev_sys_config(Name, Flags) ->
    Path = filename:join([Name, "config", "dev_sys.config.src"]),
    rebar3_nova_utils:write_file(Path, sys_config(Name, dev, Flags)).

%%======================================================================
%% prod_sys.config.src
%%======================================================================

generate_prod_sys_config(Name, Flags) ->
    Path = filename:join([Name, "config", "prod_sys.config.src"]),
    rebar3_nova_utils:write_file(Path, sys_config(Name, prod, Flags)).

%%----------------------------------------------------------------------
%% sys.config section builders
%%----------------------------------------------------------------------

sys_config(Name, Env, Flags) ->
    Sections = [
        Section
     || Section <- [
            sys_config_kernel(Env, Flags),
            sys_config_nova(Name, Env, Flags),
            sys_config_db(Name, Env, Flags),
            sys_config_arizona(Name, Flags),
            sys_config_otel(Env, Flags)
        ],
        Section =/= []
    ],
    [
        "%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-\n\n",
        "[\n",
        lists:join(",\n", Sections),
        "\n].\n"
    ].

sys_config_kernel(dev, #{lfe := true}) ->
    [
        " {kernel, [\n",
        "     {logger, [\n",
        "         {handler, default, logger_std_h,\n",
        "          #{level => info,\n",
        "            formatter => {logjam,\n",
        "              #{colored => true,\n",
        "                time_designator => $\\s,\n",
        "                time_offset => \"\",\n",
        "                time_unit => second,\n",
        "                strip_tz => true,\n",
        "                level_capitalize => true\n",
        "              }\n",
        "            }\n",
        "           }\n",
        "          }\n",
        "     ]}\n",
        " ]}"
    ];
sys_config_kernel(dev, _Flags) ->
    [
        " {kernel, [\n",
        "     {logger_level, debug},\n",
        "     {logger, [\n",
        "         {handler, default, logger_std_h, #{\n",
        "             formatter => {flatlog, #{\n",
        "                 map_depth => 3,\n",
        "                 term_depth => 50,\n",
        "                 colored => true,\n",
        "                 template => [colored_start, \"[\\033[1m\", level, \"\\033[0m\", colored_start, \"] [\", time, \"]\",\n",
        "                              colored_end, \" \", msg, \" (\", mfa, \")\\n\"]\n",
        "             }}\n",
        "         }}\n",
        "     ]}\n",
        " ]}"
    ];
sys_config_kernel(prod, _Flags) ->
    [
        " {kernel, [\n",
        "     {logger_level, info},\n",
        "     {logger, [\n",
        "         {handler, default, logger_std_h, #{\n",
        "             level => error,\n",
        "             config => #{file => \"log/erlang.log\"}\n",
        "         }}\n",
        "     ]}\n",
        " ]}"
    ].

sys_config_nova(Name, Env, Flags) ->
    {Stacktrace, EnvName} =
        case Env of
            dev -> {"true", "dev"};
            prod -> {"false", "prod"}
        end,
    [
        " {nova, [\n",
        "     {use_stacktrace, ",
        Stacktrace,
        "},\n",
        "     {environment, ",
        EnvName,
        "},\n",
        "     {cowboy_configuration, #{\n",
        "         port => 8080",
        sys_config_middlewares(Flags),
        "\n",
        "     }},\n",
        "     {bootstrap_application, ",
        Name,
        "},\n",
        "     {plugins, [\n",
        "         {pre_request, nova_request_plugin, #{decode_json_body => true}}\n",
        "     ]}\n",
        " ]}"
    ].

sys_config_middlewares(#{arizona := true}) ->
    [
        ",\n",
        "         %% arizona_nova:middlewares() spelled out: sys.config only takes terms.\n",
        "         middleware_handlers => [\n",
        "             arizona_cowboy_middleware,\n",
        "             nova_router,\n",
        "             nova_plugin_handler,\n",
        "             nova_security_handler,\n",
        "             nova_handler,\n",
        "             nova_plugin_handler\n",
        "         ]"
    ];
sys_config_middlewares(_Flags) ->
    [].

sys_config_db(Name, dev, #{kura := true}) ->
    [
        " {kura, [\n",
        "     {repos, #{\n",
        "         ",
        Name,
        "_repo => #{\n",
        "             backend => kura_backend_postgres,\n",
        "             host => \"localhost\",\n",
        "             port => 5432,\n",
        "             database => \"",
        Name,
        "_dev\",\n",
        "             user => \"postgres\",\n",
        "             password => \"postgres\",\n",
        "             pool_size => 10\n",
        "         }\n",
        "     }}\n",
        " ]}"
    ];
sys_config_db(Name, prod, #{kura := true}) ->
    [
        " {kura, [\n",
        "     %% TLS to PostgreSQL is on unless DATABASE_SSL=false, which is only for a\n",
        "     %% database on the same host or a private network. minato verifies the\n",
        "     %% server certificate against the OS trust store and checks the hostname;\n",
        "     %% for a private CA, add ssl_options => [{cacertfile, \"...\"}] to the repo.\n",
        "     %% The top-level ssl is for creating the database, the repo's for the pool.\n",
        "     {ssl, ${DATABASE_SSL:-true}},\n",
        "     {repos, #{\n",
        "         ",
        Name,
        "_repo => #{\n",
        "             backend => kura_backend_postgres,\n",
        "             host => \"${DATABASE_HOST}\",\n",
        "             port => ${DATABASE_PORT:-5432},\n",
        "             database => \"${DATABASE_NAME:-",
        Name,
        "}\",\n",
        "             user => \"${DATABASE_USER}\",\n",
        "             password => \"${DATABASE_PASSWORD}\",\n",
        "             pool_size => ${DATABASE_POOL_SIZE:-10},\n",
        "             ssl => ${DATABASE_SSL:-true}\n",
        "         }\n",
        "     }}\n",
        " ]}"
    ];
sys_config_db(Name, dev, #{pgo := true}) ->
    [
        " {pgo, [\n",
        "     {pools, [\n",
        "         {default, #{\n",
        "             pool_size => 10,\n",
        "             host => \"127.0.0.1\",\n",
        "             port => 5432,\n",
        "             database => \"",
        Name,
        "_dev\",\n",
        "             user => \"postgres\",\n",
        "             password => \"postgres\"\n",
        "         }}\n",
        "     ]}\n",
        " ]}"
    ];
sys_config_db(Name, prod, #{pgo := true}) ->
    [
        " {pgo, [\n",
        "     {pools, [\n",
        "         {default, #{\n",
        "             pool_size => ${DATABASE_POOL_SIZE:-10},\n",
        "             host => \"${DATABASE_HOST}\",\n",
        "             port => ${DATABASE_PORT:-5432},\n",
        "             database => \"${DATABASE_NAME:-",
        Name,
        "}\",\n",
        "             user => \"${DATABASE_USER}\",\n",
        "             password => \"${DATABASE_PASSWORD}\"\n",
        "         }}\n",
        "     ]}\n",
        " ]}"
    ];
sys_config_db(_Name, _Env, _Flags) ->
    [].

sys_config_arizona(Name, #{arizona := true}) ->
    [
        " {arizona_nova, [\n",
        "     %% These routes are served by arizona_cowboy_middleware, before nova_router,\n",
        "     %% so the global Nova plugins (CSRF, auth, headers, otel) do not run on them.\n",
        "     {routes, [\n",
        "         {live, ~\"/\", ",
        Name,
        "_home_live, #{layouts => [{",
        Name,
        "_layout, render}]}},\n",
        "         {ws, ~\"/live/ws\", #{}},\n",
        "         {asset, ~\"/assets/arizona\", {priv_dir, arizona, \"static/assets/js\"}}\n",
        "     ]}\n",
        " ]}"
    ];
sys_config_arizona(_Name, _Flags) ->
    [].

sys_config_otel(dev, #{otel := true}) ->
    [
        " {opentelemetry, [\n",
        "     {span_processor, simple},\n",
        "     {traces_exporter, {otel_exporter_stdout, []}}\n",
        " ]}"
    ];
sys_config_otel(prod, #{otel := true}) ->
    [
        " {opentelemetry, [\n",
        "     {span_processor, batch},\n",
        "     {traces_exporter, {opentelemetry_exporter, #{endpoints => [\"${OTEL_ENDPOINT}\"]}}}\n",
        " ]}"
    ];
sys_config_otel(_Env, _Flags) ->
    [].

%%======================================================================
%% vm.args.src
%%======================================================================

generate_vm_args(Name) ->
    Path = filename:join([Name, "config", "vm.args.src"]),
    PrivDir =
        case code:priv_dir(rebar3_nova) of
            Dir when is_list(Dir) -> Dir
        end,
    {ok, Source} = file:read_file(filename:join([PrivDir, "templates", "nova", "vm.args.src"])),
    Content = binary:replace(Source, ~"{{name}}", unicode:characters_to_binary(Name), [global]),
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% .tool-versions
%%======================================================================

generate_tool_versions(Name) ->
    Path = filename:join(Name, ".tool-versions"),
    Content = [
        "erlang 29.1.2\n",
        "rebar 3.27.0\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% .gitignore
%%======================================================================

generate_gitignore(Name) ->
    Path = filename:join(Name, ".gitignore"),
    Content = [
        ".rebar3\n",
        "_checkouts\n",
        ".eunit\n",
        "*.o\n",
        "*.beam\n",
        "*.plt\n",
        "*.swp\n",
        "*.swo\n",
        ".erlang.cookie\n",
        "ebin\n",
        "log\n",
        "erl_crash.dump\n",
        ".rebar\n",
        "logs\n",
        "_build\n",
        ".idea\n",
        "*.iml\n",
        "rebar3.crashdump\n",
        "*~\n",
        ".env\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% favicon
%%======================================================================

copy_favicon(Name) ->
    DestPath = filename:join([Name, "priv", "assets", "favicon.ico"]),
    rebar3_nova_utils:copy_priv_file(
        filename:join(["templates", "nova", "favicon.ico"]),
        DestPath
    ).

%%======================================================================
%% maybe_generate_view (ErlyDTL template, skip for arizona)
%%======================================================================

maybe_generate_view(_Name, #{arizona := true}) ->
    ok;
maybe_generate_view(Name, _Flags) ->
    DestPath = filename:join([Name, "src", "views", Name ++ "_main.dtl"]),
    rebar3_nova_utils:copy_priv_file(
        filename:join(["templates", "nova", "controller.dtl"]),
        DestPath
    ).

%%======================================================================
%% maybe_generate_kura
%%======================================================================

maybe_generate_kura(Name, #{kura := true}) ->
    generate_kura_repo(Name),
    ok = filelib:ensure_path(filename:join([Name, "src", "migrations"])),
    generate_docker_compose(Name);
maybe_generate_kura(_Name, _Flags) ->
    ok.

generate_kura_repo(Name) ->
    Path = filename:join([Name, "src", Name ++ "_repo.erl"]),
    Content = [
        "-module(",
        Name,
        "_repo).\n\n",
        "-behaviour(kura_repo).\n\n",
        "-export([otp_app/0]).\n\n",
        "otp_app() -> ",
        Name,
        ".\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% maybe_generate_pgo
%%======================================================================

maybe_generate_pgo(Name, #{pgo := true}) ->
    generate_docker_compose(Name);
maybe_generate_pgo(_Name, _Flags) ->
    ok.

%%======================================================================
%% docker-compose.yml (shared by kura and pgo)
%%======================================================================

generate_docker_compose(Name) ->
    Path = filename:join(Name, "docker-compose.yml"),
    case filelib:is_regular(Path) of
        true ->
            ok;
        false ->
            Content = [
                "services:\n",
                "  postgres:\n",
                "    image: postgres:17\n",
                "    environment:\n",
                "      POSTGRES_USER: postgres\n",
                "      POSTGRES_PASSWORD: postgres\n",
                "      POSTGRES_DB: ",
                Name,
                "_dev\n",
                "    ports:\n",
                "      - \"127.0.0.1:5432:5432\"\n",
                "    volumes:\n",
                "      - pgdata:/var/lib/postgresql/data\n\n",
                "volumes:\n",
                "  pgdata:\n"
            ],
            rebar3_nova_utils:write_file(Path, Content)
    end.

%%======================================================================
%% maybe_generate_arizona
%%======================================================================

maybe_generate_arizona(Name, #{arizona := true}) ->
    generate_home_live(Name),
    generate_live_layout(Name),
    generate_app_css(Name);
maybe_generate_arizona(_Name, _Flags) ->
    ok.

generate_home_live(Name) ->
    Path = filename:join([Name, "src", "live", Name ++ "_home_live.erl"]),
    Content = [
        "-module(",
        Name,
        "_home_live).\n",
        "-include_lib(\"arizona/include/arizona_stateful.hrl\").\n\n",
        "-export([mount/1, render/1, handle_event/3]).\n\n",
        "mount(_Bindings) ->\n",
        "    {#{id => ~\"app\", title => ~\"",
        Name,
        "\", count => 0}, #{}}.\n\n",
        "render(Bindings) ->\n",
        "    ?html(\n",
        "        {main, [{id, ?get(id)}], [\n",
        "            {h1, [], [~\"Hello from Arizona!\"]},\n",
        "            {button, [{az_click, arizona_js:push_event(~\"dec\")}], [~\"-\"]},\n",
        "            {span, [{id, ~\"count\"}], [?get(count)]},\n",
        "            {button, [{az_click, arizona_js:push_event(~\"inc\")}], [~\"+\"]}\n",
        "        ]}\n",
        "    ).\n\n",
        "handle_event(~\"inc\", _Payload, Bindings) ->\n",
        "    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []};\n",
        "handle_event(~\"dec\", _Payload, Bindings) ->\n",
        "    {Bindings#{count => maps:get(count, Bindings) - 1}, #{}, []}.\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

generate_live_layout(Name) ->
    Path = filename:join([Name, "src", "live", Name ++ "_layout.erl"]),
    Content = [
        "-module(",
        Name,
        "_layout).\n",
        "-include_lib(\"arizona/include/arizona_stateless.hrl\").\n\n",
        "-export([render/1]).\n\n",
        "render(Bindings) ->\n",
        "    ?html([\n",
        "        ~\"<!DOCTYPE html>\",\n",
        "        {html, [{lang, ~\"en\"}], [\n",
        "            {head, [], [\n",
        "                {meta, [{charset, ~\"utf-8\"}], []},\n",
        "                {meta, [{name, ~\"viewport\"}, {content, ~\"width=device-width, initial-scale=1\"}],\n",
        "                    []},\n",
        "                {title, [], [?get(title, ~\"",
        Name,
        "\")]},\n",
        "                {link, [{rel, ~\"icon\"}, {href, ~\"/assets/favicon.ico\"}], []},\n",
        "                {link, [{rel, ~\"stylesheet\"}, {href, ~\"/assets/app.css\"}], []}\n",
        "            ]},\n",
        "            {body, [], [\n",
        "                ?inner_content,\n",
        "                {script, [{type, ~\"module\"}], [\n",
        "                    ~\"\"\"\n",
        "                    import { connect } from '/assets/arizona/arizona.min.js';\n",
        "                    connect('/live/ws');\n",
        "                    \"\"\"\n",
        "                ]}\n",
        "            ]}\n",
        "        ]}\n",
        "    ]).\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

generate_app_css(Name) ->
    Path = filename:join([Name, "priv", "assets", "app.css"]),
    Content = [
        "* {\n",
        "    margin: 0;\n",
        "    padding: 0;\n",
        "    box-sizing: border-box;\n",
        "}\n\n",
        "body {\n",
        "    font-family: system-ui, -apple-system, sans-serif;\n",
        "    background: #1b2735;\n",
        "    color: #f5f6fa;\n",
        "    display: flex;\n",
        "    justify-content: center;\n",
        "    align-items: center;\n",
        "    min-height: 100vh;\n",
        "}\n\n",
        "#app {\n",
        "    text-align: center;\n",
        "}\n\n",
        "#app h1 {\n",
        "    font-size: 2rem;\n",
        "    font-weight: 300;\n",
        "    letter-spacing: 0.1em;\n",
        "}\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% maybe_generate_ci
%%======================================================================

maybe_generate_ci(Name, #{ci := true}) ->
    Path = filename:join([Name, ".github", "workflows", "ci.yml"]),
    Content = [
        "name: CI\n\n",
        "on:\n",
        "  push:\n",
        "    branches: [main, master]\n",
        "  pull_request:\n",
        "    branches: [main, master]\n\n",
        "permissions:\n",
        "  contents: read\n\n",
        "jobs:\n",
        "  ci:\n",
        "    uses: Taure/erlang-ci/.github/workflows/erlang-ci.yml@v1\n"
    ],
    rebar3_nova_utils:write_file(Path, Content);
maybe_generate_ci(_Name, _Flags) ->
    ok.

%%======================================================================
%% maybe_generate_docker
%%======================================================================

maybe_generate_docker(Name, #{docker := true}) ->
    Path = filename:join(Name, "Dockerfile"),
    Content = [
        "# The base images are pinned by digest so a rebuild gets the same bits. To\n",
        "# update one, take the digest from `docker buildx imagetools inspect <image>`.\n",
        "FROM erlang:29@sha256:eef8acf890e463896b2890a5a11666f66d0e8812545df66e6fef77883bf4d62e AS builder\n\n",
        "WORKDIR /app\n\n",
        "COPY rebar.config rebar.lock* ./\n",
        "RUN rebar3 compile --deps_only\n\n",
        "COPY . .\n",
        "RUN rebar3 as prod release\n\n",
        "FROM debian:trixie-slim@sha256:f6b08b09a05725f9431502d3b5714f5c4576e82c1a84fa1debe0c24a7e0c3ec4\n\n",
        "RUN apt-get update && apt-get install -y --no-install-recommends \\\n",
        "    ca-certificates libssl3t64 libncurses6 libstdc++6 libsctp1 && \\\n",
        "    rm -rf /var/lib/apt/lists/*\n\n",
        "RUN useradd --system --uid 10001 --home-dir /app --shell /usr/sbin/nologin app && \\\n",
        "    mkdir /app && chown app:app /app\n\n",
        "WORKDIR /app\n",
        "COPY --from=builder --chown=app:app /app/_build/prod/rel/",
        Name,
        " ./\n\n",
        "USER app\n\n",
        "EXPOSE 8080\n\n",
        "CMD [\"bin/",
        Name,
        "\", \"foreground\"]\n"
    ],
    rebar3_nova_utils:write_file(Path, Content),
    rebar3_nova_utils:write_file(filename:join(Name, ".dockerignore"), [
        "_build\n",
        "_checkouts\n",
        ".git\n",
        ".env\n",
        "log\n",
        "erl_crash.dump\n",
        "rebar3.crashdump\n"
    ]);
maybe_generate_docker(_Name, _Flags) ->
    ok.

%%======================================================================
%% Summary
%%======================================================================

print_summary(Name, Flags) ->
    rebar_api:info("~n==> Project ~s created successfully!~n", [Name]),
    rebar_api:info("Next steps:~n", []),
    rebar_api:info("  cd ~s~n", [Name]),
    NeedsDb = maps:get(kura, Flags) orelse maps:get(pgo, Flags),
    case NeedsDb of
        true ->
            rebar_api:info("  docker compose up -d~n", []);
        false ->
            ok
    end,
    rebar_api:info("  rebar3 nova serve~n", []).
