-module(rebar3_nova_test_router).

-export([routes/1, set_routes/1]).

routes(Env) ->
    case persistent_term:get({?MODULE, routes}, []) of
        ByEnv when is_function(ByEnv, 1) -> ByEnv(Env);
        Routes -> Routes
    end.

set_routes(Routes) ->
    persistent_term:put({?MODULE, routes}, Routes).
