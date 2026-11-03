-module(rebar3_nova_new_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    base_project/1,
    kura_flag/1,
    pgo_flag/1,
    arizona_flag/1,
    lfe_flag/1,
    ci_flag/1,
    docker_flag/1,
    otel_flag/1,
    erlydtl_provider_hooks/1,
    template_erlydtl_provider_hooks/1,
    combined_kura_arizona_ci/1,
    kura_pgo_mutually_exclusive/1,
    existing_dir_aborts/1,
    sys_configs_are_valid_terms/1,
    dev_logger_uses_flatlog_handler/1,
    no_unused_nova_keys/1,
    current_toolchain/1,
    generated_erlang_compiles/1,
    templates_use_current_stack/1,
    template_sys_configs_are_valid_terms/1,
    vm_args_keep_the_cookie_out/1,
    compose_binds_postgres_to_loopback/1,
    dockerfile_runs_unprivileged/1,
    erlydtl_plugin_is_pinned/1,
    gitignore_ignores_env/1,
    prod_database_uses_tls/1
]).

all() ->
    [
        base_project,
        kura_flag,
        pgo_flag,
        arizona_flag,
        lfe_flag,
        ci_flag,
        docker_flag,
        otel_flag,
        erlydtl_provider_hooks,
        template_erlydtl_provider_hooks,
        combined_kura_arizona_ci,
        kura_pgo_mutually_exclusive,
        existing_dir_aborts,
        sys_configs_are_valid_terms,
        dev_logger_uses_flatlog_handler,
        no_unused_nova_keys,
        current_toolchain,
        generated_erlang_compiles,
        templates_use_current_stack,
        template_sys_configs_are_valid_terms,
        vm_args_keep_the_cookie_out,
        compose_binds_postgres_to_loopback,
        dockerfile_runs_unprivileged,
        erlydtl_plugin_is_pinned,
        gitignore_ignores_env,
        prod_database_uses_tls
    ].

init_per_testcase(_TC, Config) ->
    PrivDir = ?config(priv_dir, Config),
    OldCwd = file:get_cwd(),
    ok = file:set_cwd(PrivDir),
    [{old_cwd, OldCwd} | Config].

end_per_testcase(_TC, Config) ->
    {ok, OldCwd} = ?config(old_cwd, Config),
    ok = file:set_cwd(OldCwd),
    Config.

%%======================================================================
%% Tests
%%======================================================================

base_project(_Config) ->
    Name = "testapp_base",
    Flags = default_flags(),
    rebar3_nova_new:generate_project(Name, Flags),

    assert_file_exists(Name, "rebar.config"),
    assert_file_exists(Name, "src/testapp_base.app.src"),
    assert_file_exists(Name, "src/testapp_base_app.erl"),
    assert_file_exists(Name, "src/testapp_base_sup.erl"),
    assert_file_exists(Name, "src/testapp_base_router.erl"),
    assert_file_exists(Name, "src/controllers/testapp_base_main_controller.erl"),
    assert_file_exists(Name, "src/views/testapp_base_main.dtl"),
    assert_file_exists(Name, "config/dev_sys.config.src"),
    assert_file_exists(Name, "config/prod_sys.config.src"),
    assert_file_exists(Name, "config/vm.args.src"),
    assert_file_exists(Name, ".tool-versions"),
    assert_file_exists(Name, ".gitignore"),
    assert_file_exists(Name, "priv/assets/favicon.ico"),

    assert_file_not_exists(Name, "docker-compose.yml"),
    assert_file_not_exists(Name, "Dockerfile"),
    assert_file_not_exists(Name, ".github/workflows/ci.yml"),

    RebarConfig = read_file(Name, "rebar.config"),
    assert_contains(RebarConfig, "nova"),
    assert_contains(RebarConfig, "flatlog"),
    assert_contains(RebarConfig, "rebar3_erlydtl_plugin"),
    assert_contains(RebarConfig, "provider_hooks"),
    assert_not_contains(RebarConfig, "kura"),
    assert_not_contains(RebarConfig, "arizona"),

    AppSrc = read_file(Name, "src/testapp_base.app.src"),
    assert_contains(AppSrc, "kernel"),
    assert_contains(AppSrc, "nova"),

    SupErl = read_file(Name, "src/testapp_base_sup.erl"),
    assert_contains(SupErl, "ChildSpecs = []"),

    ok.

kura_flag(_Config) ->
    Name = "testapp_kura",
    Flags = (default_flags())#{kura => true},
    rebar3_nova_new:generate_project(Name, Flags),

    assert_file_exists(Name, "src/testapp_kura_repo.erl"),
    assert_file_exists(Name, "docker-compose.yml"),

    ?assert(filelib:is_dir(filename:join([Name, "src", "migrations"]))),

    Terms = consult_rebar_config(Name),
    Deps = prop(deps, Terms),
    ?assertEqual({kura, "~> 2.25"}, lists:keyfind(kura, 1, Deps)),
    ?assertEqual({kura_postgres, "~> 1.1"}, lists:keyfind(kura_postgres, 1, Deps)),
    ?assertEqual(
        {rebar3_kura, "~> 0.16"},
        lists:keyfind(rebar3_kura, 1, prop(project_plugins, Terms))
    ),
    ?assertEqual(
        [{pre, [{compile, {erlydtl, compile}}, {compile, {kura, compile}}]}],
        prop(provider_hooks, Terms)
    ),
    Erlfmt = prop(erlfmt, Terms),
    ?assert(lists:member("src/migrations/*.erl", prop(exclude_files, Erlfmt))),

    AppSrc = read_file(Name, "src/testapp_kura.app.src"),
    assert_contains(AppSrc, "kura_postgres"),

    SupErl = read_file(Name, "src/testapp_kura_sup.erl"),
    assert_contains(SupErl, "ChildSpecs = []"),

    AppErl = read_file(Name, "src/testapp_kura_app.erl"),
    assert_contains(AppErl, "{ok, _} = kura_migrator:migrate(testapp_kura_repo)"),

    DevConfig = read_file(Name, "config/dev_sys.config.src"),
    assert_not_contains(DevConfig, "pgo"),
    assert_contains(DevConfig, "testapp_kura_dev"),
    {ok, [DevTerms]} = file:consult(filename:join([Name, "config", "dev_sys.config.src"])),
    #{testapp_kura_repo := #{backend := kura_backend_postgres}} =
        prop(repos, prop(kura, DevTerms)),

    RepoErl = read_file(Name, "src/testapp_kura_repo.erl"),
    assert_contains(RepoErl, "kura_repo"),
    assert_contains(RepoErl, "testapp_kura"),

    ok.

pgo_flag(_Config) ->
    Name = "testapp_pgo",
    Flags = (default_flags())#{pgo => true},
    rebar3_nova_new:generate_project(Name, Flags),

    assert_file_exists(Name, "docker-compose.yml"),
    assert_file_not_exists(Name, "src/testapp_pgo_repo.erl"),

    RebarConfig = read_file(Name, "rebar.config"),
    assert_contains(RebarConfig, "pgo"),
    assert_not_contains(RebarConfig, "kura"),

    DevConfig = read_file(Name, "config/dev_sys.config.src"),
    assert_contains(DevConfig, "pgo"),

    ok.

arizona_flag(_Config) ->
    Name = "testapp_arizona",
    Flags = (default_flags())#{arizona => true},
    rebar3_nova_new:generate_project(Name, Flags),

    assert_file_exists(Name, "src/live/testapp_arizona_home_live.erl"),
    assert_file_exists(Name, "src/live/testapp_arizona_layout.erl"),
    assert_file_exists(Name, "priv/assets/app.css"),
    assert_file_not_exists(Name, "src/views/testapp_arizona_main.dtl"),
    assert_file_not_exists(Name, "src/controllers/testapp_arizona_main_controller.erl"),

    RebarConfig = read_file(Name, "rebar.config"),
    assert_contains(RebarConfig, "{arizona_nova, \"~> 0.1\"}"),
    assert_not_contains(RebarConfig, "arizona_core"),
    assert_not_contains(RebarConfig, "erlydtl"),

    Live = read_file(Name, "src/live/testapp_arizona_home_live.erl"),
    assert_contains(Live, "arizona/include/arizona_stateful.hrl"),
    assert_not_contains(Live, "arizona_view"),

    Router = read_file(Name, "src/testapp_arizona_router.erl"),
    assert_not_contains(Router, "arizona_nova_adapter"),
    assert_not_contains(Router, "main_controller"),

    [
        assert_contains(
            read_file(Name, "config/" ++ Env ++ "_sys.config.src"), "before nova_router"
        )
     || Env <- ["dev", "prod"]
    ],
    {ok, [DevTerms]} = file:consult(filename:join([Name, "config", "dev_sys.config.src"])),
    ?assertEqual(undefined, prop(arizona_core, DevTerms)),
    #{middleware_handlers := [arizona_cowboy_middleware, nova_router | _]} =
        prop(cowboy_configuration, prop(nova, DevTerms)),
    Routes = prop(routes, prop(arizona_nova, DevTerms)),
    ?assertMatch(
        {live, <<"/">>, testapp_arizona_home_live, #{layouts := [{testapp_arizona_layout, render}]}},
        lists:keyfind(live, 1, Routes)
    ),
    ?assertMatch({ws, <<"/live/ws">>, _}, lists:keyfind(ws, 1, Routes)),
    ?assertMatch(
        {asset, <<"/assets/arizona">>, {priv_dir, arizona, "static/assets/js"}},
        lists:keyfind(asset, 1, Routes)
    ),

    ok.

lfe_flag(_Config) ->
    Name = "testapp_lfe",
    Flags = (default_flags())#{lfe => true},
    rebar3_nova_new:generate_project(Name, Flags),

    assert_file_exists(Name, "src/testapp_lfe_app.lfe"),
    assert_file_exists(Name, "src/testapp_lfe.sup.lfe"),
    assert_file_exists(Name, "src/testapp_lfe_router.lfe"),
    assert_file_exists(Name, "src/controllers/testapp_lfe_main_controller.lfe"),
    assert_file_not_exists(Name, "src/testapp_lfe_app.erl"),
    assert_file_not_exists(Name, "src/testapp_lfe_sup.erl"),

    RebarConfig = read_file(Name, "rebar.config"),
    assert_contains(RebarConfig, "logjam"),
    assert_contains(RebarConfig, "rebar3_lfe"),
    assert_contains(RebarConfig, "provider_hooks"),
    assert_not_contains(RebarConfig, "flatlog"),

    DevConfig = read_file(Name, "config/dev_sys.config.src"),
    assert_contains(DevConfig, "logjam"),

    ok.

ci_flag(_Config) ->
    Name = "testapp_ci",
    Flags = (default_flags())#{ci => true},
    rebar3_nova_new:generate_project(Name, Flags),

    assert_file_exists(Name, ".github/workflows/ci.yml"),

    CiYml = read_file(Name, ".github/workflows/ci.yml"),
    assert_contains(CiYml, "Taure/erlang-ci"),

    ok.

docker_flag(_Config) ->
    Name = "testapp_docker",
    Flags = (default_flags())#{docker => true},
    rebar3_nova_new:generate_project(Name, Flags),

    assert_file_exists(Name, "Dockerfile"),

    Dockerfile = read_file(Name, "Dockerfile"),
    assert_contains(Dockerfile, "FROM erlang:29@sha256:"),
    assert_contains(Dockerfile, "FROM debian:trixie-slim@sha256:"),
    assert_contains(Dockerfile, "ca-certificates"),
    assert_contains(Dockerfile, "testapp_docker"),
    assert_contains(Dockerfile, "EXPOSE 8080"),

    ok.

otel_flag(_Config) ->
    Name = "testapp_otel",
    Flags = (default_flags())#{otel => true},
    rebar3_nova_new:generate_project(Name, Flags),

    RebarConfig = read_file(Name, "rebar.config"),
    assert_contains(RebarConfig, "opentelemetry"),
    assert_contains(RebarConfig, "opentelemetry_nova"),

    AppSrc = read_file(Name, "src/testapp_otel.app.src"),
    assert_contains(AppSrc, "opentelemetry"),

    AppErl = read_file(Name, "src/testapp_otel_app.erl"),
    assert_contains(AppErl, "ok = opentelemetry_nova:setup()"),
    assert_not_contains(AppErl, "opentelemetry:setup()"),
    assert_contains(AppSrc, "opentelemetry_nova"),

    DevConfig = read_file(Name, "config/dev_sys.config.src"),
    assert_contains(DevConfig, "otel_exporter_stdout"),

    ProdConfig = read_file(Name, "config/prod_sys.config.src"),
    assert_contains(ProdConfig, "OTEL_ENDPOINT"),

    ok.

erlydtl_provider_hooks(_Config) ->
    Hook = {provider_hooks, [{pre, [{compile, {erlydtl, compile}}]}]},

    Base = "testapp_hooks_base",
    rebar3_nova_new:generate_project(Base, default_flags()),
    ?assert(lists:member(Hook, consult_rebar_config(Base))),

    Arizona = "testapp_hooks_arizona",
    rebar3_nova_new:generate_project(Arizona, (default_flags())#{arizona => true}),
    ?assertEqual(
        false,
        lists:keyfind(provider_hooks, 1, consult_rebar_config(Arizona))
    ),

    Lfe = "testapp_hooks_lfe",
    rebar3_nova_new:generate_project(Lfe, (default_flags())#{lfe => true}),
    ?assert(
        lists:member(
            {provider_hooks, [
                {pre, [{compile, {erlydtl, compile}}, {compile, {lfe, compile}}]}
            ]},
            consult_rebar_config(Lfe)
        )
    ),

    ok.

template_erlydtl_provider_hooks(_Config) ->
    Content = read_file(templates_dir(), "nova/rebar.config"),
    assert_contains(Content, "{provider_hooks, ["),
    assert_contains(Content, "{pre, [{compile, {erlydtl, compile}}]}"),
    ok.

combined_kura_arizona_ci(_Config) ->
    Name = "testapp_combo",
    Flags = (default_flags())#{kura => true, arizona => true, ci => true},
    rebar3_nova_new:generate_project(Name, Flags),

    assert_file_exists(Name, "src/testapp_combo_repo.erl"),
    assert_file_exists(Name, "docker-compose.yml"),
    assert_file_exists(Name, "src/live/testapp_combo_home_live.erl"),
    assert_file_exists(Name, "priv/assets/app.css"),
    assert_file_exists(Name, ".github/workflows/ci.yml"),
    assert_file_not_exists(Name, "src/views/testapp_combo_main.dtl"),

    RebarConfig = read_file(Name, "rebar.config"),
    assert_contains(RebarConfig, "kura_postgres"),
    assert_contains(RebarConfig, "arizona_nova"),
    assert_not_contains(RebarConfig, "erlydtl"),
    ?assertEqual(
        [{pre, [{compile, {kura, compile}}]}],
        prop(provider_hooks, consult_rebar_config(Name))
    ),

    AppErl = read_file(Name, "src/testapp_combo_app.erl"),
    assert_contains(AppErl, "kura_migrator:migrate(testapp_combo_repo)"),

    ok.

kura_pgo_mutually_exclusive(_Config) ->
    Flags = (default_flags())#{kura => true, pgo => true},
    ?assertMatch({error, _}, rebar3_nova_new:validate_flags(Flags)),
    ok.

existing_dir_aborts(_Config) ->
    Name = "testapp_exists",
    ok = file:make_dir(Name),
    Flags = default_flags(),
    ?assertError(
        {dir_exists, Name},
        rebar3_nova_new:generate_project(Name, Flags)
    ),
    ok.

sys_configs_are_valid_terms(_Config) ->
    Combos = [
        #{},
        #{kura => true},
        #{pgo => true},
        #{arizona => true},
        #{lfe => true},
        #{otel => true},
        #{kura => true, arizona => true, otel => true},
        #{lfe => true, arizona => true}
    ],
    [
        assert_sys_configs_parse("testapp_cfg_" ++ integer_to_list(N), Extra)
     || {N, Extra} <- lists:enumerate(Combos)
    ],
    ok.

assert_sys_configs_parse(Name, Extra) ->
    rebar3_nova_new:generate_project(Name, maps:merge(default_flags(), Extra)),
    [
        ?assertMatch({ok, [[_ | _]]}, consult_env_substituted(Name ++ "/config/" ++ File), File)
     || File <- ["dev_sys.config.src", "prod_sys.config.src"]
    ].

dev_logger_uses_flatlog_handler(_Config) ->
    Name = "testapp_logger",
    rebar3_nova_new:generate_project(Name, default_flags()),
    {ok, [Terms]} = file:consult(filename:join([Name, "config", "dev_sys.config.src"])),
    Kernel = prop(kernel, Terms),
    ?assert(lists:all(fun is_tuple/1, Kernel)),
    ?assertMatch(
        [{handler, default, logger_std_h, #{formatter := {flatlog, _}}}],
        prop(logger, Kernel)
    ).

no_unused_nova_keys(_Config) ->
    Name = "testapp_nova_keys",
    rebar3_nova_new:generate_project(Name, default_flags()),
    [
        assert_not_contains(read_file(Name, "config/" ++ File), "dev_mode")
     || File <- ["dev_sys.config.src", "prod_sys.config.src"]
    ].

current_toolchain(_Config) ->
    Name = "testapp_toolchain",
    rebar3_nova_new:generate_project(Name, default_flags()),
    ?assertEqual("erlang 29.1.2\nrebar 3.27.0\n", read_file(Name, ".tool-versions")),
    GitIgnore = string:split(read_file(Name, ".gitignore"), "\n", all),
    ?assertNot(lists:member("_*", GitIgnore)),
    ?assert(lists:member("_build", GitIgnore)),
    Terms = consult_rebar_config(Name),
    ?assertEqual({nova, "~> 0.18"}, lists:keyfind(nova, 1, prop(deps, Terms))),
    ?assertEqual(
        {rebar3_nova, "~> 0.13"},
        lists:keyfind(rebar3_nova, 1, prop(project_plugins, Terms))
    ),
    TestProfile = prop(test, prop(profiles, Terms)),
    ?assertEqual([{meck, "1.2.0"}], prop(deps, TestProfile)).

generated_erlang_compiles(_Config) ->
    Name = "testapp_compiles",
    rebar3_nova_new:generate_project(Name, (default_flags())#{kura => true, otel => true}),
    Files = filelib:wildcard(Name ++ "/src/**/*.erl"),
    ?assertNotEqual([], Files),
    [?assertMatch({ok, _, _}, compile:file(File, [binary, return_errors]), File) || File <- Files].

templates_use_current_stack(_Config) ->
    Dir = templates_dir(),
    Files = filelib:wildcard("nova*/*", Dir) ++ filelib:wildcard("nova*/.*", Dir),
    [
        ?assertEqual(nomatch, string:find(read_file(Dir, File), Stale), {File, Stale})
     || File <- Files,
        Stale <- [
            "arizona_view",
            "arizona_core",
            "Taure/arizona_nova",
            "{meck, \"1.1.0\"}",
            "rebar3_nova.git"
        ]
    ],
    [
        ?assertEqual(nomatch, string:find(read_file(Dir, File), "dev_mode"), File)
     || File <- template_sys_configs()
    ],
    {ok, Fullstack} = file:consult(Dir ++ "/nova_fullstack/rebar.config"),
    Deps = prop(deps, Fullstack),
    ?assertEqual({kura, "~> 2.25"}, lists:keyfind(kura, 1, Deps)),
    ?assertEqual({kura_postgres, "~> 1.1"}, lists:keyfind(kura_postgres, 1, Deps)),
    ?assertEqual({arizona_nova, "~> 0.1"}, lists:keyfind(arizona_nova, 1, Deps)),
    GitIgnore = string:split(read_file(Dir, "nova/.gitignore"), "\n", all),
    ?assertNot(lists:member("_*", GitIgnore)).

template_sys_configs_are_valid_terms(_Config) ->
    [
        ?assert(lists:all(fun is_tuple/1, prop(kernel, render_template_config(File))), File)
     || File <- template_sys_configs()
    ],
    [
        assert_contains(read_file(templates_dir(), "nova_fullstack/" ++ File), "before nova_router")
     || File <- ["dev_sys.config.src", "prod_sys.config.src"]
    ],
    Terms = render_template_config("nova_fullstack/dev_sys.config.src"),
    #{app_repo := #{backend := kura_backend_postgres}} =
        prop(repos, prop(kura, Terms)),
    ?assertMatch(
        [{live, <<"/">>, app_home_live, #{layouts := [{app_layout, render}]}} | _],
        prop(routes, prop(arizona_nova, Terms))
    ).

%% A cookie written into vm.args is the same for every project of that name,
%% so anyone who can reach the distribution port can run code on the node.
vm_args_keep_the_cookie_out(_Config) ->
    Name = "testapp_vmargs",
    rebar3_nova_new:generate_project(Name, default_flags()),
    Generated = read_file(Name, "config/vm.args.src"),
    assert_contains(Generated, "-sname testapp_vmargs@localhost"),
    [assert_vm_args(V) || V <- [Generated, read_file(templates_dir(), "nova/vm.args.src")]],
    assert_contains(
        read_file(templates_dir(), "nova_lfe.template"),
        "{template, \"nova/vm.args.src\", \"{{name}}/config/vm.args.src\"}"
    ),
    assert_contains(
        read_file(templates_dir(), "nova_lfe/rebar.config"),
        "{vm_args_src, \"./config/vm.args.src\"}"
    ).

%% The development database's postgres/postgres login must not be reachable
%% from the network.
compose_binds_postgres_to_loopback(_Config) ->
    Name = "testapp_compose",
    rebar3_nova_new:generate_project(Name, (default_flags())#{kura => true}),
    [
        begin
            assert_contains(Compose, "\"127.0.0.1:5432:5432\""),
            assert_not_contains(Compose, "- \"5432:5432\"")
        end
     || Compose <- [
            read_file(Name, "docker-compose.yml"),
            read_file(templates_dir(), "nova_fullstack/docker-compose.yml")
        ]
    ].

dockerfile_runs_unprivileged(_Config) ->
    Name = "testapp_dockeruser",
    rebar3_nova_new:generate_project(Name, (default_flags())#{docker => true}),
    Dockerfile = read_file(Name, "Dockerfile"),
    [_Builder, Runtime] = string:split(Dockerfile, "\nFROM ", trailing),
    assert_contains(Runtime, "useradd --system --uid 10001"),
    assert_contains(Runtime, "COPY --from=builder --chown=app:app"),
    assert_contains(Runtime, "\nUSER app\n"),
    ?assertMatch(
        {match, _},
        re:run(Dockerfile, "^FROM erlang:29@sha256:[0-9a-f]{64} AS builder$", [multiline])
    ),
    ?assertMatch({match, _}, re:run(Runtime, "^debian:trixie-slim@sha256:[0-9a-f]{64}\n")),
    Ignored = string:split(read_file(Name, ".dockerignore"), "\n", all),
    [?assert(lists:member(Entry, Ignored), Entry) || Entry <- ["_build", ".git", ".env"]].

erlydtl_plugin_is_pinned(_Config) ->
    Name = "testapp_erlydtl_pin",
    rebar3_nova_new:generate_project(Name, default_flags()),
    {ok, Template} = file:consult(filename:join(templates_dir(), "nova/rebar.config")),
    {ok, Lfe} = file:consult(filename:join(templates_dir(), "nova_lfe/rebar.config")),
    [
        ?assertMatch(
            {rebar3_erlydtl_plugin, _,
                {git, "https://github.com/erlydtl/rebar3_erlydtl_plugin.git", {ref, [_ | _]}}},
            lists:keyfind(rebar3_erlydtl_plugin, 1, Plugins)
        )
     || Plugins <- [
            prop(project_plugins, consult_rebar_config(Name)),
            prop(project_plugins, Template),
            prop(plugins, Lfe)
        ]
    ].

gitignore_ignores_env(_Config) ->
    Name = "testapp_gitignore",
    rebar3_nova_new:generate_project(Name, default_flags()),
    [
        ?assert(lists:member(".env", string:split(GitIgnore, "\n", all)))
     || GitIgnore <- [
            read_file(Name, ".gitignore"),
            read_file(templates_dir(), "nova/.gitignore"),
            read_file(templates_dir(), "nova_fullstack/.gitignore")
        ]
    ].

%% kura_postgres hands ssl to minato, which then verifies the server
%% certificate against the OS trust store.
prod_database_uses_tls(_Config) ->
    Name = "testapp_dbtls",
    rebar3_nova_new:generate_project(Name, (default_flags())#{kura => true}),
    {ok, [Generated]} = consult_env_substituted(
        filename:join([Name, "config", "prod_sys.config.src"])
    ),
    Fullstack = render_template_config("nova_fullstack/prod_sys.config.src"),
    [
        begin
            Kura = prop(kura, Terms),
            ?assertEqual(true, prop(ssl, Kura)),
            [Repo] = maps:values(prop(repos, Kura)),
            ?assertMatch(#{ssl := true}, Repo)
        end
     || Terms <- [Generated, Fullstack]
    ].


template_sys_configs() ->
    [
        "nova/dev_sys.config.src",
        "nova/prod_sys.config.src",
        "nova_lfe/dev_sys.config.src",
        "nova_fullstack/dev_sys.config.src",
        "nova_fullstack/prod_sys.config.src"
    ].

-spec render_template_config(string()) -> dynamic().
render_template_config(File) ->
    {ok, Bin} = file:read_file(templates_dir() ++ "/" ++ File),
    {ok, [Terms]} = consult_string(binary:replace(Bin, ~"{{name}}", ~"app", [global])),
    Terms.

%%======================================================================
%% Helpers
%%======================================================================

-spec prop(atom(), dynamic()) -> dynamic().
prop(Key, Terms) ->
    proplists:get_value(Key, Terms).

templates_dir() ->
    case code:priv_dir(rebar3_nova) of
        Dir when is_list(Dir) -> Dir ++ "/templates"
    end.

%% relx substitutes ${VAR} and ${VAR:-Default} before parsing a .src config.
-spec consult_env_substituted(string()) -> {ok, dynamic()} | {error, term()}.
consult_env_substituted(Path) ->
    {ok, Bin} = file:read_file(Path),
    consult_string(Bin).

-spec consult_string(binary()) -> {ok, dynamic()} | {error, term()}.
consult_string(Bin) ->
    Substituted = re:replace(Bin, "\\$\\{[A-Z_]+(:-([^}]*))?\\}", "\\2", [global, {return, binary}]),
    Path = "substituted.config",
    ok = file:write_file(Path, Substituted),
    file:consult(Path).

default_flags() ->
    #{
        kura => false,
        pgo => false,
        arizona => false,
        lfe => false,
        ci => false,
        docker => false,
        otel => false
    }.

assert_file_exists(Base, RelPath) ->
    Path = filename:join(Base, RelPath),
    ?assert(filelib:is_regular(Path), #{msg => "Expected file to exist", path => Path}).

assert_file_not_exists(Base, RelPath) ->
    Path = filename:join(Base, RelPath),
    ?assertNot(filelib:is_regular(Path), #{msg => "Expected file to not exist", path => Path}).

-spec consult_rebar_config(string()) -> dynamic().
consult_rebar_config(Base) ->
    {ok, Terms} = file:consult(filename:join(Base, "rebar.config")),
    Terms.

-spec read_file(string(), string()) -> string().
read_file(Base, RelPath) ->
    Path = filename:join(Base, RelPath),
    {ok, Bin} = file:read_file(Path),
    binary_to_list(Bin).

assert_vm_args(VmArgs) ->
    Lines = [L || L <- string:split(VmArgs, "\n", all), string:trim(L) =/= ""],
    ?assertEqual(["-setcookie ${RELEASE_COOKIE}"], [L || "-setcookie" ++ _ = L <- Lines]),
    ?assertEqual("-setcookie ${RELEASE_COOKIE}", lists:last(Lines)),
    ?assert(lists:member("-kernel inet_dist_use_interface {127,0,0,1}", Lines)).

assert_contains(Content, Substr) ->
    case string:find(Content, Substr) of
        nomatch ->
            ct:fail({expected_substring, Substr, Content});
        _ ->
            ok
    end.

assert_not_contains(Content, Substr) ->
    case string:find(Content, Substr) of
        nomatch ->
            ok;
        _ ->
            ct:fail({unexpected_substring, Substr, Content})
    end.
