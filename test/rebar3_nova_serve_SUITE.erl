-module(rebar3_nova_serve_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    watch_dirs_covers_sources_and_views/1,
    fs_backend_matches_fs/1,
    missing_inotifywait_is_reported/1,
    found_watcher_is_not_a_problem/1,
    router_edit_reloads_routes/1,
    prefixed_app_keeps_its_prefix/1,
    other_modules_leave_routes_alone/1
]).

-define(APP, rebar3_nova).
-define(ROUTER, rebar3_nova_test_router).

all() ->
    [
        watch_dirs_covers_sources_and_views,
        fs_backend_matches_fs,
        missing_inotifywait_is_reported,
        found_watcher_is_not_a_problem,
        router_edit_reloads_routes,
        prefixed_app_keeps_its_prefix,
        other_modules_leave_routes_alone
    ].

init_per_testcase(_Case, Config) ->
    _ = application:load(nova),
    _ = application:load(?APP),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(?APP, router_module, ?ROUTER),
    Config.

end_per_testcase(_Case, _Config) ->
    application:unset_env(?APP, router_module),
    _ = persistent_term:erase(nova_dispatch),
    _ = persistent_term:erase(nova_apps),
    ok.

watch_dirs_covers_sources_and_views(_Config) ->
    ?assertEqual(
        [
            "/app/src",
            "/app/src/views",
            "/app/src/controllers",
            "/app/priv",
            "/app/c_src"
        ],
        rebar3_nova_serve:watch_dirs("/app")
    ),
    ok.

fs_backend_matches_fs(_Config) ->
    ?assertEqual(inotifywait, rebar3_nova_serve:fs_backend({unix, linux})),
    ?assertEqual(fsevents, rebar3_nova_serve:fs_backend({unix, darwin})),
    ?assertEqual(kqueue, rebar3_nova_serve:fs_backend({unix, netbsd})),
    ?assertEqual(inotifywait_win32, rebar3_nova_serve:fs_backend({win32, nt})),
    ?assertEqual(undefined, rebar3_nova_serve:fs_backend({unix, sunos})).

missing_inotifywait_is_reported(_Config) ->
    "rebar3 nova serve needs inotifywait" ++ Hint =
        rebar3_nova_serve:watcher_problem({unix, linux}, false),
    ?assertNotEqual(nomatch, string:find(Hint, "inotify-tools")),
    ?assert(is_list(rebar3_nova_serve:watcher_problem({unix, sunos}, unsupported))).

found_watcher_is_not_a_problem(_Config) ->
    ?assertEqual(none, rebar3_nova_serve:watcher_problem({unix, linux}, true)).

router_edit_reloads_routes(_Config) ->
    ?ROUTER:set_routes([#{routes => [{"/old", fun test_controller:old/1, #{methods => [get]}}]}]),
    _ = nova_router:compile([?APP]),
    ?assertMatch({ok, _, _}, nova_router:lookup_url('_', ~"/old", ~"GET")),

    ?ROUTER:set_routes([#{routes => [{"/new", fun test_controller:new/1, #{methods => [get]}}]}]),
    ?assertEqual([?APP], rebar3_nova_serve:reload_routes(?ROUTER)),

    ?assertMatch({ok, _, _}, nova_router:lookup_url('_', ~"/new", ~"GET")),
    ?assertMatch({error, _}, nova_router:lookup_url('_', ~"/old", ~"GET")).

prefixed_app_keeps_its_prefix(_Config) ->
    ?ROUTER:set_routes([#{routes => [{"/a", fun test_controller:a/1, #{methods => [get]}}]}]),
    _ = nova_router:compile([{?APP, #{prefix => "/v1"}}]),
    ?ROUTER:set_routes([#{routes => [{"/b", fun test_controller:b/1, #{methods => [get]}}]}]),
    ?assertEqual([?APP], rebar3_nova_serve:reload_routes(?ROUTER)),
    ?assertMatch({ok, _, _}, nova_router:lookup_url('_', ~"/v1/b", ~"GET")),
    ?assertMatch({error, _}, nova_router:lookup_url('_', ~"/v1/a", ~"GET")).

other_modules_leave_routes_alone(_Config) ->
    ?ROUTER:set_routes([#{routes => [{"/old", fun test_controller:old/1, #{methods => [get]}}]}]),
    _ = nova_router:compile([?APP]),
    ?ROUTER:set_routes([]),
    ?assertEqual([], rebar3_nova_serve:reload_routes(test_controller)),
    ?assertMatch({ok, _, _}, nova_router:lookup_url('_', ~"/old", ~"GET")).
