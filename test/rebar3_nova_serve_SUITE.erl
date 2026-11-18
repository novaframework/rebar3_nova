-module(rebar3_nova_serve_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("nova/include/nova_router.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    watch_dirs_covers_sources_and_views/1,
    fs_backend_matches_fs/1,
    missing_inotifywait_is_reported/1,
    found_watcher_is_not_a_problem/1,
    router_edit_reloads_routes/1,
    prefixed_app_keeps_its_prefix/1,
    other_modules_leave_routes_alone/1,
    nova_apps_options_survive_a_reload/1,
    nested_nova_apps_options_survive_a_reload/1,
    changed_table/1,
    gave_up/1,
    table_being_built/1
]).

-define(APP, rebar3_nova).
-define(ROUTER, rebar3_nova_test_router).
-define(BOOT, rebar3_nova_serve_boot).
-define(PARENT, rebar3_nova_serve_parent).

all() ->
    [
        watch_dirs_covers_sources_and_views,
        fs_backend_matches_fs,
        missing_inotifywait_is_reported,
        found_watcher_is_not_a_problem,
        router_edit_reloads_routes,
        prefixed_app_keeps_its_prefix,
        other_modules_leave_routes_alone,
        nova_apps_options_survive_a_reload,
        nested_nova_apps_options_survive_a_reload,
        changed_table,
        gave_up,
        table_being_built
    ].

init_per_testcase(_Case, Config) ->
    _ = application:load(nova),
    _ = application:load(?APP),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(?APP, router_module, ?ROUTER),
    Config.

end_per_testcase(_Case, _Config) ->
    application:unset_env(?APP, router_module),
    application:unset_env(nova, bootstrap_application),
    application:unset_env(?BOOT, nova_apps),
    application:unset_env(?PARENT, nova_apps),
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

%% Nova 1.0 refuses a compile when routes were stored into the table as the
%% router ran. The routes of the application are out of the table by then,
%% so they are compiled again.
changed_table(_Config) ->
    Old = [#{routes => [{"/old", fun test_controller:old/1, #{methods => [get]}}]}],
    New = [#{routes => [{"/new", fun test_controller:new/1, #{methods => [get]}}]}],
    Side = #{prefix => "/side", routes => [{"/", fun test_controller:a/1, #{methods => [get]}}]},
    ?ROUTER:set_routes(Old),
    _ = nova_router:compile_fresh([?APP], nova_dispatch),
    ?ROUTER:set_routes(fun(_Env) ->
        case put(?FUNCTION_NAME, ran) of
            undefined ->
                ok = stored(other_app, Side),
                New;
            ran ->
                New
        end
    end),
    try
        ?assertEqual([?APP], rebar3_nova_serve:reload_routes(?ROUTER))
    after
        erase(?FUNCTION_NAME)
    end,
    ?assertMatch({ok, _, _}, nova_router:lookup_url('_', ~"/new", ~"GET")),
    ?assertMatch({ok, _, _}, nova_router:lookup_url('_', ~"/side/", ~"GET")),
    ?assertMatch({error, _}, nova_router:lookup_url('_', ~"/old", ~"GET")).

%% A table that changes each time the routes are compiled is given up on.
%% The routes of the application are out of it by then, and a warning
%% says so.
gave_up(_Config) ->
    Old = [#{routes => [{"/old", fun test_controller:old/1, #{methods => [get]}}]}],
    New = [#{routes => [{"/new", fun test_controller:new/1, #{methods => [get]}}]}],
    Side = fun(N) ->
        #{
            prefix => "/side" ++ integer_to_list(N),
            routes => [{"/", fun test_controller:a/1, #{methods => [get]}}]
        }
    end,
    ?ROUTER:set_routes(Old),
    _ = nova_router:compile_fresh([?APP], nova_dispatch),
    ?ROUTER:set_routes(fun(_Env) ->
        Ran =
            case get(?FUNCTION_NAME) of
                undefined -> 1;
                Before -> Before + 1
            end,
        put(?FUNCTION_NAME, Ran),
        ok = stored(other_app, Side(Ran)),
        New
    end),
    try
        ?assertEqual([?APP], rebar3_nova_serve:reload_routes(?ROUTER)),
        ?assertEqual(3, get(?FUNCTION_NAME))
    after
        erase(?FUNCTION_NAME)
    end,
    ?assertMatch({error, _}, nova_router:lookup_url('_', ~"/new", ~"GET")),
    ?assertMatch({error, _}, nova_router:lookup_url('_', ~"/old", ~"GET")),
    ?assertMatch({ok, _, _}, nova_router:lookup_url('_', ~"/side3/", ~"GET")).

%% A router does not store into the table it is compiled into, so what is
%% stored as it runs is stored by another process.
stored(App, Routes) ->
    {Storing, Monitor} = spawn_monitor(fun() ->
        exit({stored, nova_router:add_routes(App, Routes)})
    end),
    receive
        {'DOWN', Monitor, process, Storing, {stored, Stored}} -> Stored;
        {'DOWN', Monitor, process, Storing, NotStored} -> error(NotStored)
    after 5000 -> error(not_stored)
    end.

%% While the listener of the table is started again, Nova 1.0 refuses to
%% take an application out of the table before it. The start compiles the
%% routes of the application itself, so the reload leaves them to it.
table_being_built(_Config) ->
    Test = self(),
    Old = [#{routes => [{"/old", fun test_controller:old/1, #{methods => [get]}}]}],
    Built = [#{routes => [{"/built", fun test_controller:new/1, #{methods => [get]}}]}],
    ?ROUTER:set_routes(Old),
    _ = nova_router:compile_fresh([?APP], nova_dispatch),
    ?ROUTER:set_routes(fun(_Env) ->
        Test ! building,
        receive
            go -> Built
        after 5000 -> error(not_let_go)
        end
    end),
    {Builder, Monitor} = spawn_monitor(fun() -> nova_router:compile_fresh([?APP], nova_dispatch) end),
    receive
        building -> ok
    after 5000 -> error(not_building)
    end,
    ?assertEqual([?APP], rebar3_nova_serve:reload_routes(?ROUTER)),
    Builder ! go,
    receive
        {'DOWN', Monitor, process, Builder, Reason} -> ?assertEqual(normal, Reason)
    after 5000 -> error(not_built)
    end,
    ?assertMatch({ok, _, _}, nova_router:lookup_url('_', ~"/built", ~"GET")).

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

%% A sub-app's security and plugins come from its nova_apps entry, not its
%% router; reloading with the prefix alone dropped them.
nova_apps_options_survive_a_reload(_Config) ->
    application:set_env(nova, bootstrap_application, ?BOOT),
    application:set_env(?BOOT, nova_apps, [{?APP, sub_app_options()}]),
    assert_reload_keeps(sub_app_options()).

nested_nova_apps_options_survive_a_reload(_Config) ->
    application:set_env(nova, bootstrap_application, ?BOOT),
    application:set_env(?BOOT, nova_apps, [?PARENT]),
    application:set_env(?PARENT, nova_apps, [{?APP, sub_app_options()}]),
    assert_reload_keeps(sub_app_options()).

sub_app_options() ->
    #{
        prefix => "/admin",
        secure => fun test_security:deny/1,
        plugin_strategy => {override, [{pre_request, nova_request_plugin, #{}}]}
    }.

assert_reload_keeps(Options) ->
    ?ROUTER:set_routes([#{routes => [{"/a", fun test_controller:a/1, #{methods => [get]}}]}]),
    _ = nova_router:compile([{?APP, Options}]),
    ?ROUTER:set_routes([#{routes => [{"/b", fun test_controller:b/1, #{methods => [get]}}]}]),
    ?assertEqual([?APP], rebar3_nova_serve:reload_routes(?ROUTER)),
    {ok, _, Value} = nova_router:lookup_url('_', ~"/admin/b", ~"GET"),
    #nova_handler_value{secure = Secure, plugins = Plugins} = rebar3_nova_dispatch:payload(Value),
    ?assertEqual(fun test_security:deny/1, Secure),
    ?assert(lists:member({fun nova_request_plugin:pre_request/4, #{}}, callbacks(Plugins))).

%% Nova 0.18 keeps pre_request/4 in the pre_request phase, a later one in a
%% phase behind the security callback.
callbacks(Plugins) ->
    [Callback || {_Phase, Callbacks} <- Plugins, is_list(Callbacks), Callback <- Callbacks].
