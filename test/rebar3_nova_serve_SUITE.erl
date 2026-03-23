-module(rebar3_nova_serve_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    is_routefile_true/1,
    is_routefile_false/1,
    is_routefile_empty/1,
    is_routefile_partial_match/1
]).

all() ->
    [
        is_routefile_true,
        is_routefile_false,
        is_routefile_empty,
        is_routefile_partial_match
    ].

is_routefile_true(_Config) ->
    ?assert(rebar3_nova_serve:is_routefile("src/myapp.routes.erl")),
    ?assert(rebar3_nova_serve:is_routefile("/full/path/to/app.routes.erl")),
    ?assert(rebar3_nova_serve:is_routefile(".routes.erl")).

is_routefile_false(_Config) ->
    ?assertNot(rebar3_nova_serve:is_routefile("src/myapp_controller.erl")),
    ?assertNot(rebar3_nova_serve:is_routefile("src/views/main.dtl")),
    ?assertNot(rebar3_nova_serve:is_routefile("rebar.config")).

is_routefile_empty(_Config) ->
    ?assertNot(rebar3_nova_serve:is_routefile("")).

is_routefile_partial_match(_Config) ->
    ?assertNot(rebar3_nova_serve:is_routefile("routes.erl")),
    ?assertNot(rebar3_nova_serve:is_routefile(".routes")),
    ?assertNot(rebar3_nova_serve:is_routefile("routes")).
