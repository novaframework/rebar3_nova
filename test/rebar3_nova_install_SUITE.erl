-module(rebar3_nova_install_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    creates_config_with_hex_plugin/1,
    keeps_comments_when_adding_to_plugins/1,
    adds_plugins_to_existing_config/1,
    leaves_existing_nova_plugin_alone/1,
    installer_runs_only_a_verified_rebar3/1,
    installer_is_fetched_from_a_tag/1
]).

all() ->
    [
        creates_config_with_hex_plugin,
        keeps_comments_when_adding_to_plugins,
        adds_plugins_to_existing_config,
        leaves_existing_nova_plugin_alone,
        installer_runs_only_a_verified_rebar3,
        installer_is_fetched_from_a_tag
    ].

creates_config_with_hex_plugin(Config) ->
    Home = home(Config, "none"),
    install(Home),
    ?assertEqual(
        {ok, [{plugins, [{rebar3_nova, "~> 0.13"}]}]},
        file:consult(global_config(Home))
    ).

keeps_comments_when_adding_to_plugins(Config) ->
    Home = home(Config, "comments"),
    write_global_config(Home, "%% keep me\n{plugins, [\n    rebar3_hex %% inline\n]}.\n"),
    install(Home),
    {ok, Bin} = file:read_file(global_config(Home)),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"%% keep me")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"%% inline")),
    ?assertEqual(
        {ok, [{plugins, [{rebar3_nova, "~> 0.13"}, rebar3_hex]}]},
        file:consult(global_config(Home))
    ).

adds_plugins_to_existing_config(Config) ->
    Home = home(Config, "no_plugins"),
    write_global_config(Home, "%% mine\n{erl_opts, [debug_info]}.\n"),
    install(Home),
    {ok, Bin} = file:read_file(global_config(Home)),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"%% mine")),
    ?assertEqual(
        {ok, [{erl_opts, [debug_info]}, {plugins, [{rebar3_nova, "~> 0.13"}]}]},
        file:consult(global_config(Home))
    ).

leaves_existing_nova_plugin_alone(Config) ->
    Home = home(Config, "has_nova"),
    Existing = "{plugins, [{rebar3_nova, \"0.13.3\"}]}.\n",
    write_global_config(Home, Existing),
    install(Home),
    ?assertEqual({ok, list_to_binary(Existing)}, file:read_file(global_config(Home))).

%% The old installer ran `./rebar3` from the working directory after an
%% unverified download, so a planted or tampered file ran instead.
installer_runs_only_a_verified_rebar3(_Config) ->
    Escript = repo_file("install.escript"),
    [
        ?assertEqual(nomatch, string:find(Escript, Bad), Bad)
     || Bad <- ["wget", "\"./rebar3", "s3.amazonaws"]
    ],
    ?assertMatch(
        {match, _}, re:run(Escript, "-define\\(REBAR3_SHA256, \"[0-9a-f]{64}\"\\)")
    ),
    ?assertNotEqual(nomatch, string:find(Escript, "{verify, verify_peer}")),
    ?assertNotEqual(nomatch, string:find(Escript, "file:make_dir(Dir)")).

installer_is_fetched_from_a_tag(_Config) ->
    Sh = repo_file("install.sh"),
    ?assertNotEqual(nomatch, string:find(Sh, "curl -fsSL")),
    ?assertMatch({match, _}, re:run(Sh, "REBAR3_NOVA_REF:-v[0-9]+\\.[0-9]+\\.[0-9]+")),
    ?assertEqual(nomatch, string:find(Sh, "/master/")),
    ?assertEqual(nomatch, string:find(repo_file("README.md"), "rebar3_nova/master/install.sh")).

repo_file(Name) ->
    {ok, Bin} = file:read_file(filename:join([lib_dir(), "..", "..", "..", "..", Name])),
    binary_to_list(Bin).

home(Config, Name) ->
    Home = filename:join(?config(priv_dir, Config), Name),
    ok = filelib:ensure_path(Home),
    Home.

global_config(Home) ->
    filename:join([Home, ".config", "rebar3", "rebar.config"]).

write_global_config(Home, Content) ->
    Path = global_config(Home),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Content).

install(Home) ->
    Script = filename:join([lib_dir(), "..", "..", "..", "..", "install.escript"]),
    true = filelib:is_regular(Script),
    Port = open_port(
        {spawn_executable, os:find_executable("escript")},
        [{args, [Script]}, {env, [{"HOME", Home}]}, exit_status, stderr_to_stdout]
    ),
    ?assertEqual(0, wait_for_exit(Port)).

lib_dir() ->
    case code:lib_dir(rebar3_nova) of
        Dir when is_list(Dir) -> Dir
    end.

wait_for_exit(Port) ->
    receive
        {Port, {data, _}} -> wait_for_exit(Port);
        {Port, {exit_status, Status}} -> Status
    after 30000 -> error(install_timeout)
    end.
