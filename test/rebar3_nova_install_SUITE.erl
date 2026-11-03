-module(rebar3_nova_install_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-export([all/0]).
-export([
    creates_config_with_hex_plugin/1,
    keeps_comments_when_adding_to_plugins/1,
    adds_plugins_to_existing_config/1,
    leaves_existing_nova_plugin_alone/1,
    installer_runs_only_a_verified_rebar3/1,
    installer_is_fetched_from_a_tag/1,
    installer_verifies_the_download/1,
    installer_downloads_into_an_unguessable_dir/1,
    installer_exits_non_zero_on_errors/1
]).

all() ->
    [
        creates_config_with_hex_plugin,
        keeps_comments_when_adding_to_plugins,
        adds_plugins_to_existing_config,
        leaves_existing_nova_plugin_alone,
        installer_runs_only_a_verified_rebar3,
        installer_is_fetched_from_a_tag,
        installer_verifies_the_download,
        installer_downloads_into_an_unguessable_dir,
        installer_exits_non_zero_on_errors
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

installer_verifies_the_download(Config) ->
    Installer = installer_module(Config),
    Good = ~"a rebar3 escript",
    Sha = binary_to_list(binary:encode_hex(crypto:hash(sha256, Good), lowercase)),
    ?assertEqual({ok, Good}, Installer:verified(Good, Sha)),
    ?assertMatch({error, {checksum_mismatch, _}}, Installer:verified(<<Good/binary, 0>>, Sha)),
    ?assertMatch({error, {checksum_mismatch, _}}, Installer:verified(Good)).

%% A predictable name in a shared /tmp can be created first by another user.
installer_downloads_into_an_unguessable_dir(Config) ->
    Installer = installer_module(Config),
    Dirs = [Installer:make_private_dir() || _ <- [1, 2]],
    try
        ?assertMatch([A, B] when A =/= B, Dirs),
        [
            begin
                ?assertMatch(
                    {match, _}, re:run(filename:basename(Dir), "^rebar3_nova_install_[0-9a-f]{32}$")
                ),
                {ok, #file_info{mode = Mode}} = file:read_file_info(Dir),
                ?assertEqual(8#700, Mode band 8#777)
            end
         || Dir <- Dirs
        ]
    after
        [file:del_dir_r(Dir) || Dir <- Dirs]
    end.

%% `bash -c "$(curl ...)" && ...` and CI scripts need the exit status. With
%% stdin closed, the rebar3 prompt used to loop on eof forever.
installer_exits_non_zero_on_errors(Config) ->
    Broken = home(Config, "broken"),
    write_global_config(Broken, "{plugins, [\n"),
    ?assertEqual("1", run_installer(Broken, os:getenv("PATH"))),
    NoRebar3 = home(Config, "no_rebar3"),
    Path = filename:join(code:root_dir(), "bin") ++ ":/usr/bin:/bin",
    ?assertEqual(false, os:find_executable("rebar3", Path)),
    ?assertEqual("1", run_installer(NoRebar3, Path)).

run_installer(Home, Path) ->
    Cmd = io_lib:format(
        "cd '~ts' && HOME='~ts' PATH='~ts' '~ts' 30 '~ts' '~ts' </dev/null >/dev/null 2>&1; echo $?",
        [
            Home,
            Home,
            Path,
            os:find_executable("timeout"),
            filename:join([code:root_dir(), "bin", "escript"]),
            repo_path("install.escript")
        ]
    ),
    string:trim(os:cmd(lists:flatten(Cmd))).

%% The escript body, compiled as a module so its functions can be called.
installer_module(Config) ->
    {ok, Sections} = escript:extract(repo_path("install.escript"), []),
    {source, Source} = lists:keyfind(source, 1, Sections),
    Path = filename:join(?config(priv_dir, Config), "rebar3_nova_installer.erl"),
    ok = file:write_file(Path, ["-module(rebar3_nova_installer).\n", Source]),
    {ok, Forms} = epp:parse_file(Path, []),
    {ok, Mod, Bin} = compile:forms(Forms, [export_all, nowarn_export_all]),
    {module, Mod} = code:load_binary(Mod, Path, Bin),
    Mod.

repo_path(Name) ->
    filename:join([lib_dir(), "..", "..", "..", "..", Name]).

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
