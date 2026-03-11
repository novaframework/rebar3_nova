-module(rebar3_nova_fly).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, fly).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova fly deploy"},
        {opts, [
            {action, undefined, undefined, string, "Action: init, deploy, or status"}
        ]},
        {short_desc, "Deploy Nova application to Fly.io"},
        {desc,
            "Manage Fly.io deployments for Nova applications.\n\n"
            "Actions:\n"
            "  init    - Create a new Fly.io app (generates fly.toml if missing)\n"
            "  deploy  - Build and deploy to Fly.io\n"
            "  status  - Show app status on Fly.io\n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ensure_flyctl(),
    {Args, _} = rebar_state:command_parsed_args(State),
    Action = proplists:get_value(action, Args),
    AppName = rebar3_nova_utils:get_app_name(State),
    AppDir = rebar3_nova_utils:get_app_dir(State),
    run_action(Action, AppName, AppDir, State).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%--------------------------------------------------------------------
%% Actions
%%--------------------------------------------------------------------

run_action("init", AppName, AppDir, State) ->
    fly_init(AppName, AppDir, State);
run_action("deploy", _AppName, AppDir, State) ->
    fly_deploy(AppDir, State);
run_action("status", _AppName, _AppDir, State) ->
    fly_status(State);
run_action(undefined, _AppName, AppDir, State) ->
    fly_deploy(AppDir, State);
run_action(Other, _AppName, _AppDir, _State) ->
    rebar_api:abort("Unknown action: ~s. Use init, deploy, or status.", [Other]).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

ensure_flyctl() ->
    case os:find_executable("fly") of
        false ->
            rebar_api:abort(
                "flyctl not found. Install it: curl -L https://fly.io/install.sh | sh", []);
        _Path ->
            ok
    end.

fly_init(AppName, AppDir, State) ->
    maybe_generate_fly_toml(AppName, AppDir),
    rebar_api:info("Creating Fly.io app...", []),
    Cmd = ["cd ", AppDir, " && fly launch --no-deploy --copy-config --yes"],
    require_cmd(Cmd, "fly launch"),
    rebar_api:info("Fly.io app created. Run 'rebar3 nova fly deploy' to deploy.", []),
    {ok, State}.

fly_deploy(AppDir, State) ->
    rebar_api:info("Deploying to Fly.io...", []),
    ToolVersions = filename:join(AppDir, ".tool-versions"),
    BuildArgs = build_args_from_tool_versions(ToolVersions),
    Cmd = ["cd ", AppDir, " && fly deploy", BuildArgs],
    require_cmd(Cmd, "fly deploy"),
    rebar_api:info("Deploy complete!", []),
    {ok, State}.

fly_status(State) ->
    require_cmd("fly status", "fly status"),
    {ok, State}.

maybe_generate_fly_toml(AppName, AppDir) ->
    FlyToml = filename:join(AppDir, "fly.toml"),
    case filelib:is_regular(FlyToml) of
        true ->
            rebar_api:info("fly.toml already exists, launching app...", []);
        false ->
            rebar_api:info("Generating fly.toml for ~s", [AppName]),
            generate_fly_toml(AppName, FlyToml)
    end.

generate_fly_toml(AppName, Path) ->
    Content = [
        "app = '", atom_to_list(AppName), "'\n",
        "primary_region = 'ams'\n",
        "\n",
        "[build]\n",
        "\n",
        "[http_service]\n",
        "  internal_port = 8080\n",
        "  force_https = true\n",
        "  auto_stop_machines = 'stop'\n",
        "  auto_start_machines = true\n",
        "  min_machines_running = 0\n",
        "  processes = ['app']\n",
        "\n",
        "[checks]\n",
        "  [checks.health]\n",
        "    grace_period = \"10s\"\n",
        "    interval = \"15s\"\n",
        "    method = \"GET\"\n",
        "    path = \"/healthz\"\n",
        "    port = 8080\n",
        "    timeout = \"5s\"\n",
        "    type = \"http\"\n",
        "\n",
        "  [checks.ready]\n",
        "    grace_period = \"15s\"\n",
        "    interval = \"15s\"\n",
        "    method = \"GET\"\n",
        "    path = \"/readyz\"\n",
        "    port = 8080\n",
        "    timeout = \"5s\"\n",
        "    type = \"http\"\n",
        "\n",
        "[[vm]]\n",
        "  size = 'shared-cpu-1x'\n",
        "  memory = '512mb'\n"
    ],
    ok = file:write_file(Path, Content),
    rebar_api:info("Created ~s", [Path]).

require_cmd(Cmd, Label) ->
    case run_cmd(Cmd) of
        0 -> ok;
        Code -> rebar_api:abort("~s failed with exit code ~p", [Label, Code])
    end.

build_args_from_tool_versions(ToolVersions) ->
    case file:read_file(ToolVersions) of
        {ok, Bin} ->
            Lines = string:split(binary_to_list(Bin), "\n", all),
            lists:flatmap(fun tool_version_to_arg/1, Lines);
        {error, _} ->
            []
    end.

tool_version_to_arg(Line) ->
    case string:tokens(string:trim(Line), " ") of
        ["erlang", Vsn] ->
            [" --build-arg ERLANG_VERSION=", Vsn];
        ["rebar", Vsn] ->
            [" --build-arg REBAR_VERSION=", Vsn];
        _ ->
            []
    end.

run_cmd(Cmd) ->
    Port = open_port({spawn, lists:flatten(Cmd)}, [
        exit_status, binary, stderr_to_stdout, {line, 1024}
    ]),
    collect_port_output(Port).

collect_port_output(Port) ->
    receive
        {Port, {data, {_, Line}}} ->
            rebar_api:info("~s", [Line]),
            collect_port_output(Port);
        {Port, {exit_status, Status}} ->
            Status
    end.
