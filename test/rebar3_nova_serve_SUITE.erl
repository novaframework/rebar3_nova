-module(rebar3_nova_serve_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([watch_dirs_covers_sources_and_views/1]).

all() ->
    [watch_dirs_covers_sources_and_views].

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
