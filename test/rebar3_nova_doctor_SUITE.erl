-module(rebar3_nova_doctor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([end_per_testcase/2]).
-export([all/0]).
-export([
    security_env_needs_an_exact_gitignore_line/1,
    security_flags_a_literal_cookie/1,
    security_asks_git_about_env/1,
    parse_csv_empty/1,
    parse_csv_single/1,
    parse_csv_multiple/1,
    parse_csv_trims_whitespace/1,
    section_status_all_ok/1,
    section_status_with_warning/1,
    section_status_with_error/1,
    section_status_error_dominates_warn/1,
    section_status_empty/1,
    summarize_clean/1,
    summarize_with_errors/1,
    summarize_warnings_only/1,
    summarize_strict_promotes_warnings/1,
    summarize_strict_clean/1,
    config_json_lib_is_read_from_the_bootstrap_app/1,
    config_raw_otp_json_is_an_error/1,
    config_json_lib_under_nova_warns/1,
    config_session_manager_default_is_ok/1,
    config_session_backend_warns/1,
    config_sessions_disabled/1
]).

all() ->
    [
        parse_csv_empty,
        parse_csv_single,
        parse_csv_multiple,
        parse_csv_trims_whitespace,
        section_status_all_ok,
        section_status_with_warning,
        section_status_with_error,
        section_status_error_dominates_warn,
        section_status_empty,
        summarize_clean,
        summarize_with_errors,
        summarize_warnings_only,
        summarize_strict_promotes_warnings,
        summarize_strict_clean,
        config_json_lib_is_read_from_the_bootstrap_app,
        config_raw_otp_json_is_an_error,
        config_json_lib_under_nova_warns,
        config_session_manager_default_is_ok,
        config_session_backend_warns,
        config_sessions_disabled,
        security_env_needs_an_exact_gitignore_line,
        security_flags_a_literal_cookie,
        security_asks_git_about_env
    ].

end_per_testcase(_Case, _Config) ->
    true = os:unsetenv("GIT_CEILING_DIRECTORIES"),
    ok.

%%======================================================================
%% parse_csv/1
%%======================================================================

parse_csv_empty(_Config) ->
    ?assertEqual([], rebar3_nova_doctor:parse_csv(undefined)),
    ?assertEqual([], rebar3_nova_doctor:parse_csv("")).

parse_csv_single(_Config) ->
    ?assertEqual(["routes"], rebar3_nova_doctor:parse_csv("routes")).

parse_csv_multiple(_Config) ->
    ?assertEqual(
        ["toolchain", "routes", "build"],
        rebar3_nova_doctor:parse_csv("toolchain,routes,build")
    ).

parse_csv_trims_whitespace(_Config) ->
    ?assertEqual(
        ["toolchain", "routes"],
        rebar3_nova_doctor:parse_csv("toolchain , routes")
    ).

%%======================================================================
%% section_status/1
%%======================================================================

section_status_all_ok(_Config) ->
    Findings = [{ok, "a"}, {ok, "b", "hint"}],
    ?assertEqual(ok, rebar3_nova_doctor:section_status(Findings)).

section_status_with_warning(_Config) ->
    Findings = [{ok, "a"}, {warn, "b", "hint"}],
    ?assertEqual(warn, rebar3_nova_doctor:section_status(Findings)).

section_status_with_error(_Config) ->
    Findings = [{ok, "a"}, {error, "bad", "fix it"}],
    ?assertEqual(error, rebar3_nova_doctor:section_status(Findings)).

section_status_error_dominates_warn(_Config) ->
    Findings = [{warn, "w"}, {error, "e"}, {warn, "w2"}],
    ?assertEqual(error, rebar3_nova_doctor:section_status(Findings)).

section_status_empty(_Config) ->
    ?assertEqual(ok, rebar3_nova_doctor:section_status([])).

%%======================================================================
%% summarize/1 — exit-code logic
%%======================================================================

summarize_clean(_Config) ->
    ?assertEqual(ok, rebar3_nova_doctor:summarize({5, 0, 0, false})).

summarize_with_errors(_Config) ->
    Result = rebar3_nova_doctor:summarize({3, 1, 2, false}),
    ?assertMatch({error, _}, Result),
    {error, Msg} = Result,
    ?assert(string:find(Msg, "2 doctor error") =/= nomatch).

summarize_warnings_only(_Config) ->
    %% Warnings without --strict do not fail.
    ?assertEqual(ok, rebar3_nova_doctor:summarize({5, 3, 0, false})).

summarize_strict_promotes_warnings(_Config) ->
    Result = rebar3_nova_doctor:summarize({5, 2, 0, true}),
    ?assertMatch({error, _}, Result),
    {error, Msg} = Result,
    ?assert(string:find(Msg, "2 doctor warning") =/= nomatch),
    ?assert(string:find(Msg, "strict") =/= nomatch).

summarize_strict_clean(_Config) ->
    %% --strict with zero warnings and zero errors is still ok.
    ?assertEqual(ok, rebar3_nova_doctor:summarize({5, 0, 0, true})).

%%======================================================================
%% config_findings/3
%%======================================================================

config_json_lib_is_read_from_the_bootstrap_app(_Config) ->
    Config = [{nova, [{bootstrap_application, blog}]}, {blog, [{json_lib, my_json}]}],
    ?assertEqual("json_lib = my_json", hd(ok_messages(Config))),
    ?assertEqual([], [F || F <- findings(Config, ok), element(1, F) =/= ok]).

config_raw_otp_json_is_an_error(_Config) ->
    Config = [{nova, [{bootstrap_application, blog}]}, {blog, [{json_lib, json}]}],
    ?assertMatch([{error, _, _} | _], findings(Config, raw_json_unsupported)),
    ?assertMatch([{error, _, _} | _], findings(Config, missing)),
    ?assertMatch([{error, _, _} | _], findings(Config, incomplete)),
    ?assertMatch([{ok, _} | _], findings(Config, ok)).

config_json_lib_under_nova_warns(_Config) ->
    Config = [{nova, [{bootstrap_application, blog}, {json_lib, json}]}],
    ?assertMatch([{warn, "json_lib is set under nova", _}], warnings(Config)).

config_session_manager_default_is_ok(_Config) ->
    Config = [{nova, [{bootstrap_application, blog}]}],
    ?assertEqual([], warnings(Config)),
    ?assert(lists:member("session_manager = nova_session_ets", ok_messages(Config))).

config_session_backend_warns(_Config) ->
    Config = [{nova, [{bootstrap_application, blog}, {session_backend, ets}]}],
    ?assertMatch([{warn, "session_backend is not a Nova setting", _}], warnings(Config)).

config_sessions_disabled(_Config) ->
    Config = [{nova, [{bootstrap_application, blog}]}, {blog, [{use_sessions, false}]}],
    ?assertEqual([], warnings(Config)),
    ?assertMatch([_], [M || "sessions disabled" ++ _ = M <- ok_messages(Config)]).

findings(Config, JsonStatus) ->
    rebar3_nova_doctor:config_findings(Config, blog, JsonStatus).

warnings(Config) ->
    [F || F <- findings(Config, ok), element(1, F) =/= ok].

ok_messages(Config) ->
    [lists:flatten(Msg) || {ok, Msg} <- findings(Config, ok)].

%%======================================================================
%% security_findings/1
%%======================================================================

security_env_needs_an_exact_gitignore_line(Config) ->
    outside_git(Config),
    Dir = project(Config, "env", [{".env", "SECRET=1\n"}, {".gitignore", "_build\n.envrc\n"}]),
    ?assertMatch([{error, ".env present but not gitignored", _}], env_findings(Dir)),
    ok = file:write_file(filename:join(Dir, ".gitignore"), "_build\n.env\n"),
    ?assertEqual([{ok, ".env present and gitignored"}], env_findings(Dir)),
    ok = file:write_file(filename:join(Dir, ".gitignore"), "/.env\n"),
    ?assertEqual([{ok, ".env present and gitignored"}], env_findings(Dir)).

%% A warning, not an error: projects from rebar3_nova 0.13.4 and earlier
%% have a literal cookie and should still pass doctor.
security_flags_a_literal_cookie(Config) ->
    Literal = project(Config, "cookie", [
        {"config/vm.args.src", "-name app\n-setcookie app_cookie\n"}
    ]),
    ?assertMatch([{warn, _, _}], cookie_findings(Literal)),
    FromEnv = project(Config, "env_cookie", [
        {"config/vm.args.src", "-name app\n-setcookie ${RELEASE_COOKIE}\n"}
    ]),
    ?assertEqual([], cookie_findings(FromEnv)),
    [{warn, _, Hint}] = cookie_findings(Literal),
    ?assertEqual(nomatch, string:find(Hint, "drop the line")),
    ?assertNotEqual(nomatch, string:find(Hint, "set RELEASE_COOKIE")),
    None = project(Config, "no_cookie", [{"config/vm.args.src", "-name app\n## -setcookie x\n"}]),
    ?assertEqual([], cookie_findings(None)).

%% A pattern only git understands.
security_asks_git_about_env(Config) ->
    outside_git(Config),
    Dir = project(Config, "git_env", [{".env", "SECRET=1\n"}, {".gitignore", ".env*\n"}]),
    ?assertMatch([{error, ".env present but not gitignored", _}], env_findings(Dir)),
    [] = os:cmd("git init -q " ++ Dir),
    ?assertEqual([{ok, ".env present and gitignored"}], env_findings(Dir)),
    ok = file:write_file(filename:join(Dir, ".gitignore"), ".env*\n!.env\n"),
    ?assertMatch([{error, ".env present but not gitignored", _}], env_findings(Dir)).

%% The suite runs inside this repository, which ignores _build; git must
%% not look above the test projects.
outside_git(Config) ->
    true = os:putenv("GIT_CEILING_DIRECTORIES", ?config(priv_dir, Config)).

project(Config, Name, Files) ->
    Dir = filename:join(?config(priv_dir, Config), Name),
    [write(filename:join(Dir, File), Content) || {File, Content} <- Files],
    Dir.

write(Path, Content) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Content).

env_findings(Dir) ->
    [
        F
     || F <- rebar3_nova_doctor:security_findings(Dir),
        string:find(element(2, F), ".env") =/= nomatch
    ].

cookie_findings(Dir) ->
    [
        F
     || {_Level, Message, _} = F <- rebar3_nova_doctor:security_findings(Dir),
        string:find(Message, "cookie") =/= nomatch
    ].
