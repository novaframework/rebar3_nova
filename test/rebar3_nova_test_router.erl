-module(rebar3_nova_test_router).

-export([routes/1, set_routes/1]).

routes(_Env) ->
    persistent_term:get({?MODULE, routes}, []).

set_routes(Routes) ->
    persistent_term:put({?MODULE, routes}, Routes).
