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
    config_environment_default_follows_nova/1,
    default_environment_leaves_a_set_environment_alone/1,
    config_lists_the_settings_nova_checks/1,
    config_leaves_checked_settings_out_for_a_nova_without_them/1,
    config_never_shows_a_secret/1,
    config_points_out_a_removed_setting/1,
    release_runs_as_the_profile/1
]).

-define(SETTINGS, rebar3_nova_test_settings).
-define(SECRET, ~"a secret that only the test suites use 0123456789").

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
        config_environment_default_follows_nova,
        default_environment_leaves_a_set_environment_alone,
        config_lists_the_settings_nova_checks,
        config_leaves_checked_settings_out_for_a_nova_without_them,
        config_never_shows_a_secret,
        config_points_out_a_removed_setting,
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
    [{nova, Nova}, {app, App}] = rebar3_nova_config:config_sections([], app),
    ?assertEqual(
        {default, rebar3_nova_utils:default_json_lib()}, proplists:get_value(json_lib, App)
    ),
    ?assertEqual(missing, proplists:get_value(bootstrap_application, Nova)),
    ?assertEqual({default, true}, proplists:get_value(render_error_pages, Nova)),
    ?assertEqual({default, false}, proplists:get_value(use_strict_routing, Nova)),
    ?assertEqual({default, nova_session_ets}, proplists:get_value(session_manager, Nova)),
    ?assertEqual({default, #{default => #{port => 8080}}}, proplists:get_value(listeners, Nova)),
    ?assertEqual(undefined, proplists:get_value(cowboy_configuration, Nova)),
    Legacy = [{nova, [{cowboy_configuration, #{port => 9090}}]}],
    [{nova, Before}, {app, _}] = rebar3_nova_config:config_sections(Legacy, app),
    ?assertEqual({set, #{port => 9090}}, proplists:get_value(cowboy_configuration, Before)),
    ?assertEqual(undefined, proplists:get_value(listeners, Before)).

%% Nova 0.18 and earlier read an unset environment as dev. A Nova that has
%% is_dev_environment/0 reads it as prod.
config_environment_default_follows_nova(_Config) ->
    _ = code:ensure_loaded(nova),
    Expected =
        case erlang:function_exported(nova, is_dev_environment, 0) of
            true -> prod;
            false -> dev
        end,
    ?assertEqual(Expected, rebar3_nova_utils:default_environment()),
    [{nova, Nova}, _] = rebar3_nova_config:config_sections([], app),
    ?assertEqual({default, Expected}, proplists:get_value(environment, Nova)).

default_environment_leaves_a_set_environment_alone(_Config) ->
    Default = rebar3_nova_utils:default_environment(),
    ok = application:set_env(nova, environment, staging),
    try
        ?assertEqual(Default, rebar3_nova_utils:default_environment()),
        ?assertEqual({ok, staging}, application:get_env(nova, environment))
    after
        application:unset_env(nova, environment)
    end,
    ?assertEqual(Default, rebar3_nova_utils:default_environment()),
    ?assertEqual(undefined, application:get_env(nova, environment)).

config_lists_the_settings_nova_checks(_Config) ->
    Config = [{nova, [{trusted_origins, same_origin}]}],
    [{nova, Nova}, _] = rebar3_nova_config:config_sections(Config, app, ?SETTINGS),
    ?assertEqual({set, same_origin}, proplists:get_value(trusted_origins, Nova)),
    ?assertEqual({default, reject}, proplists:get_value(non_canonical_paths, Nova)),
    ?assertEqual({default, undefined}, proplists:get_value(csrf_token, Nova)),
    ?assertEqual({default, undefined}, proplists:get_value(secret_key_base, Nova)),
    ?assertEqual({default, 1000000}, proplists:get_value(max_sessions, Nova)),
    ?assertEqual({default, []}, proplists:get_value(plugins_before_security, Nova)).

config_leaves_checked_settings_out_for_a_nova_without_them(_Config) ->
    Config = [{nova, [{trusted_origins, same_origin}, {csrf_secret, ?SECRET}]}],
    [{nova, Nova}, _] = rebar3_nova_config:config_sections(Config, app, undefined),
    ?assertEqual(
        [],
        [
            Key
         || {Key, _} <- Nova, lists:member(Key, [trusted_origins, secret_key_base, csrf_secret])
        ]
    ),
    Listed = rebar3_nova_config:config_sections([], app),
    Checked = rebar3_nova_config:config_sections([], app, rebar3_nova_utils:settings_module()),
    ?assertEqual(Checked, Listed).

config_never_shows_a_secret(_Config) ->
    Config = [{nova, [{secret_key_base, ?SECRET}, {csrf_secret, ?SECRET}]}],
    Sections = rebar3_nova_config:config_sections(Config, app, ?SETTINGS),
    [{nova, Nova}, _] = Sections,
    ?assertEqual(secret, proplists:get_value(secret_key_base, Nova)),
    Printed = iolist_to_binary([rebar3_nova_config:format_section(S) || S <- Sections]),
    ?assertEqual(nomatch, binary:match(Printed, ?SECRET)),
    ?assertNotEqual(nomatch, binary:match(Printed, ~"secret_key_base")).

config_points_out_a_removed_setting(_Config) ->
    Config = [{nova, [{allowed_origins, any}]}],
    [{nova, Nova}, _] = rebar3_nova_config:config_sections(Config, app, ?SETTINGS),
    ?assertEqual({removed, trusted_origins}, proplists:get_value(allowed_origins, Nova)),
    ?assertEqual(none, proplists:lookup(csrf_secret, Nova)).

release_runs_as_the_profile(_Config) ->
    ?assertEqual(["prod", "release"], rebar3_nova_release:as_args("prod")).

route(Route) ->
    lists:flatten(io_lib:format("~ts", [rebar3_nova_middleware:format_route(Route)])).
