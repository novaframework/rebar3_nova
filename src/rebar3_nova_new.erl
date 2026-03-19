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
    case resolve_name(Opts, Args) of
        {error, missing_name} ->
            rebar_api:abort(
                "Missing project name.~n~n"
                "Usage: rebar3 nova new <name> [flags]~n~n"
                "Example:~n"
                "  rebar3 nova new myapp~n"
                "  rebar3 nova new myapp --kura --ci~n"
                "  rebar3 nova new myapp --arizona --docker~n~n"
                "Flags:~n"
                "  --kura      Include Kura database layer (PostgreSQL ORM)~n"
                "  --pgo       Include PGO PostgreSQL client (raw SQL)~n"
                "  --arizona   Include Arizona live views~n"
                "  --lfe       Generate LFE source files~n"
                "  --ci        Generate GitHub Actions CI workflow~n"
                "  --docker    Generate Dockerfile~n"
                "  --otel      Include OpenTelemetry instrumentation~n",
                []
            );
        {ok, Name} ->
            Flags = parse_flags(Opts),
            case validate_flags(Flags) of
                ok ->
                    case filelib:is_dir(Name) of
                        true ->
                            rebar_api:abort(
                                "Directory '~s' already exists. Choose a different name or remove it first.",
                                [Name]
                            );
                        false ->
                            generate_project(Name, Flags),
                            print_summary(Name, Flags),
                            {ok, State}
                    end;
                {error, Reason} ->
                    rebar_api:abort("~s", [Reason])
            end
    end.

-spec format_error(any()) -> iolist().
format_error(missing_project_name) ->
    "Missing project name. Usage: rebar3 nova new <name> [flags]";
format_error({dir_exists, Name}) ->
    io_lib:format("Directory '~s' already exists", [Name]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%======================================================================
%% Flag parsing
%%======================================================================

resolve_name(_Opts, Args) ->
    case Args of
        [N | _] -> {ok, N};
        _ -> {error, missing_name}
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
    {error, "--kura and --pgo are mutually exclusive (kura uses pgo internally)"};
validate_flags(_) ->
    ok.

%%======================================================================
%% Project generation
%%======================================================================

generate_project(Name, Flags) ->
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
            true -> ["    nova,\n", "    {logjam, \"1.2.4\"}"];
            false -> ["    nova,\n", "    {flatlog, \"0.1.2\"}"]
        end,
    KuraDep =
        case maps:get(kura, Flags) of
            true -> [",\n    kura"];
            false -> []
        end,
    PgoDep =
        case maps:get(pgo, Flags) of
            true -> [",\n    pgo"];
            false -> []
        end,
    ArizonaDeps =
        case maps:get(arizona, Flags) of
            true -> [",\n    arizona_core,\n    arizona_nova"];
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
                    ",\n    opentelemetry,\n    opentelemetry_api,\n    opentelemetry_exporter,\n    opentelemetry_nova"
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
        "            {meck, \"1.1.0\"}\n",
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
                    "        {git, \"https://github.com/erlydtl/rebar3_erlydtl_plugin.git\", {branch, \"master\"}}},\n"
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
    [
        "{project_plugins, [\n",
        ErlydtlPlugin,
        LfePlugin,
        "    {rebar3_nova, \".*\",\n",
        "        {git, \"https://github.com/novaframework/rebar3_nova.git\", {branch, \"master\"}}},\n",
        "    {erlfmt, \"~>1.7\"}\n",
        "]}.\n\n"
    ].

rebar_erlfmt(_Flags) ->
    [
        "{erlfmt, [\n",
        "    write,\n",
        "    {files, [\n",
        "        \"rebar.config\",\n",
        "        \"src/*.app.src\",\n",
        "        \"src/**/{*.erl, *.hrl}\",\n",
        "        \"test/**/{*.erl, *.hrl}\"\n",
        "    ]},\n",
        "    {exclude_files, [\"config/vm.args.src\", \"config/prod_sys.config.src\"]}\n",
        "]}.\n\n"
    ].

rebar_provider_hooks(#{lfe := true}) ->
    [
        "{provider_hooks, [\n",
        "    {pre, [{compile, {erlydtl, compile}},\n",
        "           {compile, {lfe, compile}}]}\n",
        "]}.\n\n"
    ];
rebar_provider_hooks(#{arizona := true}) ->
    [];
rebar_provider_hooks(_) ->
    [].

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
            true -> [",\n        kura"];
            false -> []
        end,
    Pgo =
        case maps:get(pgo, Flags) of
            true -> [",\n        pgo"];
            false -> []
        end,
    Arizona =
        case maps:get(arizona, Flags) of
            true -> [",\n        arizona_core,\n        arizona_nova"];
            false -> []
        end,
    Otel =
        case maps:get(otel, Flags) of
            true -> [",\n        opentelemetry,\n        opentelemetry_api"];
            false -> []
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
generate_app(Name, #{otel := true}) ->
    Path = filename:join([Name, "src", Name ++ "_app.erl"]),
    Content = [
        "-module(",
        Name,
        "_app).\n\n",
        "-behaviour(application).\n\n",
        "-export([start/2, stop/1]).\n\n",
        "start(_StartType, _StartArgs) ->\n",
        "    opentelemetry:setup(),\n",
        "    ",
        Name,
        "_sup:start_link().\n\n",
        "stop(_State) ->\n",
        "    ok.\n"
    ],
    rebar3_nova_utils:write_file(Path, Content);
generate_app(Name, _Flags) ->
    Path = filename:join([Name, "src", Name ++ "_app.erl"]),
    Content = [
        "-module(",
        Name,
        "_app).\n\n",
        "-behaviour(application).\n\n",
        "-export([start/2, stop/1]).\n\n",
        "start(_StartType, _StartArgs) ->\n",
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
generate_sup(Name, #{kura := true}) ->
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
        "    ChildSpecs = [\n",
        "        #{id => ",
        Name,
        "_repo,\n",
        "          start => {",
        Name,
        "_repo, start_link, []},\n",
        "          type => worker}\n",
        "    ],\n",
        "    {ok, {SupFlags, ChildSpecs}}.\n"
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
        "            #(\"/\" ,(lambda (params) (",
        Name,
        "_main_controller:index params))\n",
        "              #M(methods (get)))\n",
        "            ))\n",
        "      #M(prefix \"\"\n",
        "         security false\n",
        "         routes\n",
        "           (\n",
        "            #(\"/ws\" ,(lambda (params) (arizona_nova_adapter:handler params))\n",
        "              #M(protocol ws))\n",
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
        "        },\n",
        "        #{\n",
        "            prefix => \"\",\n",
        "            security => false,\n",
        "            routes => [\n",
        "                {\"/ws\", fun arizona_nova_adapter:handler/1, #{protocol => ws}}\n",
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
    Content = [
        "%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-\n\n",
        "[\n",
        sys_config_kernel(dev, Flags),
        sys_config_nova(Name, dev, Flags),
        sys_config_pgo(Name, dev, Flags),
        sys_config_arizona(Name, dev, Flags),
        sys_config_otel(dev, Flags),
        "].\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% prod_sys.config.src
%%======================================================================

generate_prod_sys_config(Name, Flags) ->
    Path = filename:join([Name, "config", "prod_sys.config.src"]),
    Content = [
        "%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-\n\n",
        "[\n",
        sys_config_kernel(prod, Flags),
        sys_config_nova(Name, prod, Flags),
        sys_config_pgo(Name, prod, Flags),
        sys_config_arizona(Name, prod, Flags),
        sys_config_otel(prod, Flags),
        "].\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%----------------------------------------------------------------------
%% sys.config section builders
%%----------------------------------------------------------------------

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
        " ]},\n"
    ];
sys_config_kernel(dev, _Flags) ->
    [
        " {kernel, [\n",
        "     {logger_level, debug},\n",
        "     #{formatter => {flatlog, #{\n",
        "         map_depth => 3,\n",
        "         term_depth => 50,\n",
        "         colored => true,\n",
        "         template => [colored_start, \"[\\033[1m\", level, \"\\033[0m\", colored_start,\"] [\", time, \"]\",\n",
        "                      colored_end, \" \", msg, \" (\", mfa, \")\\n\"]\n",
        "     }}}\n",
        " ]},\n"
    ];
sys_config_kernel(prod, _Flags) ->
    [
        " {kernel, [\n",
        "     {logger_level, info},\n",
        "     {logger,\n",
        "      [{handler, default, logger_std_h,\n",
        "        #{level => error,\n",
        "          config => #{file => \"log/erlang.log\"}}}\n",
        "      ]}\n",
        " ]},\n"
    ].

sys_config_nova(Name, dev, _Flags) ->
    [
        " {nova, [\n",
        "     {use_stacktrace, true},\n",
        "     {environment, dev},\n",
        "     {cowboy_configuration, #{\n",
        "         port => 8080\n",
        "     }},\n",
        "     {dev_mode, true},\n",
        "     {bootstrap_application, ",
        Name,
        "},\n",
        "     {plugins, [\n",
        "         {pre_request, nova_request_plugin, #{decode_json_body => true}}\n",
        "     ]}\n",
        " ]},\n"
    ];
sys_config_nova(Name, prod, _Flags) ->
    [
        " {nova, [\n",
        "     {use_stacktrace, false},\n",
        "     {environment, prod},\n",
        "     {cowboy_configuration, #{\n",
        "         port => 8080\n",
        "     }},\n",
        "     {dev_mode, false},\n",
        "     {bootstrap_application, ",
        Name,
        "},\n",
        "     {plugins, [\n",
        "         {pre_request, nova_request_plugin, #{decode_json_body => true}}\n",
        "     ]}\n",
        " ]},\n"
    ].

sys_config_pgo(Name, dev, #{kura := true}) ->
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
        " ]},\n"
    ];
sys_config_pgo(Name, dev, #{pgo := true}) ->
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
        " ]},\n"
    ];
sys_config_pgo(Name, prod, #{kura := true}) ->
    [
        " {pgo, [\n",
        "     {pools, [\n",
        "         {default, #{\n",
        "             pool_size => ${PGO_POOL_SIZE},\n",
        "             host => \"${DATABASE_HOST}\",\n",
        "             port => ${DATABASE_PORT},\n",
        "             database => \"",
        Name,
        "\",\n",
        "             user => \"${DATABASE_USER}\",\n",
        "             password => \"${DATABASE_PASSWORD}\"\n",
        "         }}\n",
        "     ]}\n",
        " ]},\n"
    ];
sys_config_pgo(Name, prod, #{pgo := true}) ->
    [
        " {pgo, [\n",
        "     {pools, [\n",
        "         {default, #{\n",
        "             pool_size => ${PGO_POOL_SIZE},\n",
        "             host => \"${DATABASE_HOST}\",\n",
        "             port => ${DATABASE_PORT},\n",
        "             database => \"",
        Name,
        "\",\n",
        "             user => \"${DATABASE_USER}\",\n",
        "             password => \"${DATABASE_PASSWORD}\"\n",
        "         }}\n",
        "     ]}\n",
        " ]},\n"
    ];
sys_config_pgo(_Name, _Env, _Flags) ->
    [].

sys_config_arizona(Name, _Env, #{arizona := true}) ->
    [
        " {arizona_core, [\n",
        "     {otp_app, ",
        Name,
        "},\n",
        "     {endpoint, #{\n",
        "         live_reload => true\n",
        "     }}\n",
        " ]},\n"
    ];
sys_config_arizona(_Name, _Env, _Flags) ->
    [].

sys_config_otel(dev, #{otel := true}) ->
    [
        " {opentelemetry, [\n",
        "     {span_processor, simple},\n",
        "     {traces_exporter, {otel_exporter_stdout, []}}\n",
        " ]},\n"
    ];
sys_config_otel(prod, #{otel := true}) ->
    [
        " {opentelemetry, [\n",
        "     {span_processor, batch},\n",
        "     {traces_exporter, {opentelemetry_exporter, #{endpoints => [\"${OTEL_ENDPOINT}\"]}}}\n",
        " ]},\n"
    ];
sys_config_otel(_Env, _Flags) ->
    [].

%%======================================================================
%% vm.args.src
%%======================================================================

generate_vm_args(Name) ->
    Path = filename:join([Name, "config", "vm.args.src"]),
    Content = [
        "-sname '",
        Name,
        "'\n\n",
        "-setcookie ",
        Name,
        "_cookie\n\n",
        "+K true\n",
        "+A30\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% .tool-versions
%%======================================================================

generate_tool_versions(Name) ->
    Path = filename:join(Name, ".tool-versions"),
    Content = [
        "erlang 28.0.4\n",
        "rebar 3.25.0\n"
    ],
    rebar3_nova_utils:write_file(Path, Content).

%%======================================================================
%% .gitignore
%%======================================================================

generate_gitignore(Name) ->
    Path = filename:join(Name, ".gitignore"),
    Content = [
        ".rebar3\n",
        "_*\n",
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
        "*~\n"
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
                "      - \"5432:5432\"\n",
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
    generate_home_view(Name),
    generate_app_css(Name);
maybe_generate_arizona(_Name, _Flags) ->
    ok.

generate_home_view(Name) ->
    Path = filename:join([Name, "src", "views", Name ++ "_home_view.erl"]),
    Content = [
        "-module(",
        Name,
        "_home_view).\n",
        "-compile({parse_transform, arizona_parse_transform}).\n",
        "-behaviour(arizona_view).\n\n",
        "-export([mount/2, render/1]).\n\n",
        "mount(_MountArg, _Request) ->\n",
        "    arizona_view:new(?MODULE, #{message => ~\"Hello from Arizona!\"}, none).\n\n",
        "render(Bindings) ->\n",
        "    arizona_template:from_html(~\"\"\"\n",
        "    <div id=\"app\">\n",
        "        <h1>{arizona_template:get_binding(message, Bindings)}</h1>\n",
        "    </div>\n",
        "    \"\"\").\n"
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
        "FROM erlang:28 AS builder\n\n",
        "WORKDIR /app\n\n",
        "COPY rebar.config rebar.lock ./\n",
        "RUN rebar3 compile\n\n",
        "COPY . .\n",
        "RUN rebar3 as prod release\n\n",
        "FROM debian:bookworm-slim\n\n",
        "RUN apt-get update && apt-get install -y --no-install-recommends \\\n",
        "    libssl3 libncurses6 libstdc++6 && \\\n",
        "    rm -rf /var/lib/apt/lists/*\n\n",
        "WORKDIR /app\n",
        "COPY --from=builder /app/_build/prod/rel/",
        Name,
        " ./\n\n",
        "EXPOSE 8080\n\n",
        "CMD [\"bin/",
        Name,
        "\", \"foreground\"]\n"
    ],
    rebar3_nova_utils:write_file(Path, Content);
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
    case maps:get(kura, Flags) of
        true ->
            rebar_api:info("  rebar3 kura setup~n", []);
        false ->
            ok
    end,
    rebar_api:info("  rebar3 nova serve~n", []).
