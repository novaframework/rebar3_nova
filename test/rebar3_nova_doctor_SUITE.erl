-module(rebar3_nova_doctor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
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
    check_session_default_warns/1,
    check_session_explicit_ets_ok/1,
    check_session_custom_manager_ok/1,
    check_session_disabled_ok/1
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
        check_session_default_warns,
        check_session_explicit_ets_ok,
        check_session_custom_manager_ok,
        check_session_disabled_ok
    ].

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
%% check_session/1
%%======================================================================

check_session_default_warns(_Config) ->
    ?assertMatch({warn, _, _}, rebar3_nova_doctor:check_session([])).

check_session_explicit_ets_ok(_Config) ->
    ?assertMatch(
        {ok, _},
        rebar3_nova_doctor:check_session([{session_manager, nova_session_ets}])
    ).

check_session_custom_manager_ok(_Config) ->
    ?assertMatch(
        {ok, _},
        rebar3_nova_doctor:check_session([{session_manager, my_redis_session}])
    ).

check_session_disabled_ok(_Config) ->
    ?assertMatch({ok, _}, rebar3_nova_doctor:check_session([{use_sessions, false}])),
    %% use_sessions=false wins even if a manager is also configured.
    ?assertMatch(
        {ok, _},
        rebar3_nova_doctor:check_session([
            {use_sessions, false},
            {session_manager, nova_session_ets}
        ])
    ).
