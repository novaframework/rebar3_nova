-module(rebar3_nova_middleware_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    format_handler_tuple/1,
    format_handler_fun/1,
    format_methods_single/1,
    format_methods_multiple/1,
    format_methods_wildcard/1
]).

all() ->
    [
        format_handler_tuple,
        format_handler_fun,
        format_methods_single,
        format_methods_multiple,
        format_methods_wildcard
    ].

format_handler_tuple(_Config) ->
    Result = lists:flatten(rebar3_nova_middleware:format_handler({myapp_controller, index})),
    ?assertEqual("myapp_controller:index", Result).

format_handler_fun(_Config) ->
    Fun = fun erlang:is_atom/1,
    Result = lists:flatten(rebar3_nova_middleware:format_handler(Fun)),
    ?assertNotEqual(nomatch, string:find(Result, "erlang")),
    ?assertNotEqual(nomatch, string:find(Result, "is_atom")).

format_methods_single(_Config) ->
    Result = rebar3_nova_middleware:format_methods([get]),
    ?assertEqual("GET", Result).

format_methods_multiple(_Config) ->
    Result = rebar3_nova_middleware:format_methods([get, post, delete]),
    ?assertEqual("GET,POST,DELETE", Result).

format_methods_wildcard(_Config) ->
    Result = rebar3_nova_middleware:format_methods(['_']),
    ?assertEqual("_", Result).
