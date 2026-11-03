-module(rebar3_nova_commands_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    middleware_formats_controller_routes/1,
    middleware_formats_static_routes/1,
    middleware_formats_status_code_routes/1,
    middleware_formats_websocket_routes/1,
    middleware_formats_inline_funs_and_unknown_routes/1,
    config_reads_each_key_from_its_app/1,
    config_flags_bootstrap_keys_set_under_nova/1,
    config_lists_current_nova_keys/1,
    release_runs_as_the_profile/1
]).

all() ->
    [
        middleware_formats_controller_routes,
        middleware_formats_static_routes,
        middleware_formats_status_code_routes,
        middleware_formats_websocket_routes,
        middleware_formats_inline_funs_and_unknown_routes,
        config_reads_each_key_from_its_app,
        config_flags_bootstrap_keys_set_under_nova,
        config_lists_current_nova_keys,
        release_runs_as_the_profile
    ].

middleware_formats_controller_routes(_Config) ->
    ?assertEqual(
        "GET,POST /users -> test_controller:index",
        route({"/users", fun test_controller:index/1, #{methods => [get, post]}})
    ),
    ?assertEqual(
        "DELETE /users/:id -> test_controller:delete",
        route({"/users/:id", {test_controller, delete}, #{methods => [~"delete"]}})
    ),
    ?assertEqual(
        "ALL /any -> test_controller:index", route({"/any", fun test_controller:index/1, #{}})
    ).

middleware_formats_static_routes(_Config) ->
    ?assertEqual("STATIC /assets/[...] -> assets", route({"/assets/[...]", "assets"})),
    ?assertEqual("STATIC /robots.txt -> robots.txt", route({"/robots.txt", "robots.txt", #{}})).

middleware_formats_status_code_routes(_Config) ->
    ?assertEqual(
        "STATUS 404 -> test_controller:not_found",
        route({404, fun test_controller:not_found/1, #{}})
    ).

middleware_formats_websocket_routes(_Config) ->
    ?assertEqual("WS /ws -> test_ws (websocket)", route({"/ws", test_ws, #{protocol => ws}})).

middleware_formats_inline_funs_and_unknown_routes(_Config) ->
    ?assertEqual(
        "GET /heartbeat -> fun in rebar3_nova_commands_SUITE",
        route({"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}})
    ),
    ?assertEqual("{something,odd,entirely,4}", route({something, odd, entirely, 4})).

config_reads_each_key_from_its_app(_Config) ->
    Config = [
        {nova, [{bootstrap_application, blog}, {session_manager, my_sessions}]},
        {blog, [{json_lib, my_json}, {use_sessions, false}]}
    ],
    [{nova, Nova}, {blog, Blog}] = rebar3_nova_config:config_sections(Config, blog),
    ?assertEqual({set, my_sessions}, proplists:get_value(session_manager, Nova)),
    ?assertEqual({set, my_json}, proplists:get_value(json_lib, Blog)),
    ?assertEqual({set, false}, proplists:get_value(use_sessions, Blog)),
    ?assertEqual(none, proplists:lookup(json_lib, Nova)).

config_flags_bootstrap_keys_set_under_nova(_Config) ->
    Config = [{nova, [{bootstrap_application, blog}, {json_lib, json}]}],
    [{nova, _}, {blog, Blog}] = rebar3_nova_config:config_sections(Config, blog),
    ?assertEqual({misplaced, json, blog}, proplists:get_value(json_lib, Blog)).

config_lists_current_nova_keys(_Config) ->
    [{nova, Nova}, {app, _}] = rebar3_nova_config:config_sections([], app),
    ?assertEqual(missing, proplists:get_value(bootstrap_application, Nova)),
    ?assertEqual({default, true}, proplists:get_value(render_error_pages, Nova)),
    ?assertEqual({default, false}, proplists:get_value(use_strict_routing, Nova)),
    ?assertEqual({default, nova_session_ets}, proplists:get_value(session_manager, Nova)).

release_runs_as_the_profile(_Config) ->
    ?assertEqual(["prod", "release"], rebar3_nova_release:as_args("prod")).

route(Route) ->
    lists:flatten(io_lib:format("~ts", [rebar3_nova_middleware:format_route(Route)])).
