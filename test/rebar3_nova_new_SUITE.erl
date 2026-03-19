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
    combined_kura_arizona_ci/1,
    kura_pgo_mutually_exclusive/1,
    existing_dir_aborts/1
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
        combined_kura_arizona_ci,
        kura_pgo_mutually_exclusive,
        existing_dir_aborts
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

    RebarConfig = read_file(Name, "rebar.config"),
    assert_contains(RebarConfig, "kura"),

    AppSrc = read_file(Name, "src/testapp_kura.app.src"),
    assert_contains(AppSrc, "kura"),

    AppErl = read_file(Name, "src/testapp_kura_app.erl"),
    assert_contains(AppErl, "kura_repo_worker:start(testapp_kura_repo)"),
    assert_contains(AppErl, "kura_migrator:migrate(testapp_kura_repo)"),

    DevConfig = read_file(Name, "config/dev_sys.config.src"),
    assert_contains(DevConfig, "pgo"),
    assert_contains(DevConfig, "testapp_kura_dev"),

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

    assert_file_exists(Name, "src/views/testapp_arizona_home_view.erl"),
    assert_file_exists(Name, "priv/static/assets/app.css"),
    assert_file_not_exists(Name, "src/views/testapp_arizona_main.dtl"),

    HomeView = read_file(Name, "src/views/testapp_arizona_home_view.erl"),
    assert_contains(HomeView, "arizona_parse_transform"),
    assert_contains(HomeView, "arizona_view:new(?MODULE"),
    assert_contains(HomeView, "arizona_template:from_erl"),
    assert_contains(HomeView, "layout(Bindings)"),
    assert_contains(HomeView, "render_slot"),

    RebarConfig = read_file(Name, "rebar.config"),
    assert_contains(RebarConfig, "arizona_core"),
    assert_contains(RebarConfig, "arizona_nova"),
    assert_not_contains(RebarConfig, "erlydtl"),

    Router = read_file(Name, "src/testapp_arizona_router.erl"),
    assert_contains(Router, "_main_controller:index/1"),

    DevConfig = read_file(Name, "config/dev_sys.config.src"),
    assert_contains(DevConfig, "arizona_core"),

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
    assert_contains(Dockerfile, "erlang:28"),
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
    assert_contains(AppErl, "opentelemetry:setup()"),

    DevConfig = read_file(Name, "config/dev_sys.config.src"),
    assert_contains(DevConfig, "otel_exporter_stdout"),

    ProdConfig = read_file(Name, "config/prod_sys.config.src"),
    assert_contains(ProdConfig, "OTEL_ENDPOINT"),

    ok.

combined_kura_arizona_ci(_Config) ->
    Name = "testapp_combo",
    Flags = (default_flags())#{kura => true, arizona => true, ci => true},
    rebar3_nova_new:generate_project(Name, Flags),

    assert_file_exists(Name, "src/testapp_combo_repo.erl"),
    assert_file_exists(Name, "docker-compose.yml"),
    assert_file_exists(Name, "src/views/testapp_combo_home_view.erl"),
    assert_file_exists(Name, "priv/static/assets/app.css"),
    assert_file_exists(Name, ".github/workflows/ci.yml"),
    assert_file_not_exists(Name, "src/views/testapp_combo_main.dtl"),

    RebarConfig = read_file(Name, "rebar.config"),
    assert_contains(RebarConfig, "kura"),
    assert_contains(RebarConfig, "arizona_core"),
    assert_not_contains(RebarConfig, "erlydtl"),

    Router = read_file(Name, "src/testapp_combo_router.erl"),
    assert_contains(Router, "_main_controller:index/1"),

    SupErl = read_file(Name, "src/testapp_combo_sup.erl"),
    assert_contains(SupErl, "supervisor"),

    ok.

kura_pgo_mutually_exclusive(_Config) ->
    Flags = (default_flags())#{kura => true, pgo => true},
    ?assertMatch({error, _}, rebar3_nova_new:validate_flags(Flags)),
    ok.

existing_dir_aborts(_Config) ->
    %% The dir-exists check is now in do/1 which calls rebar_api:abort.
    %% Verify that generate_project still works (overwrites) if called directly.
    Name = "testapp_exists",
    ok = file:make_dir(Name),
    Flags = default_flags(),
    %% generate_project no longer raises — it just generates into the existing dir
    rebar3_nova_new:generate_project(Name, Flags),
    ?assert(filelib:is_regular(filename:join(Name, "rebar.config"))),
    ok.

%%======================================================================
%% Helpers
%%======================================================================

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

read_file(Base, RelPath) ->
    Path = filename:join(Base, RelPath),
    {ok, Bin} = file:read_file(Path),
    binary_to_list(Bin).

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
