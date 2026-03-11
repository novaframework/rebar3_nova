-module({{name}}_router).
-behaviour(nova_router).

-export([
    routes/1
]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                {"/", fun {{name}}_main_controller:index/1, #{methods => [get]}},
                {"/healthz", fun {{name}}_health_controller:healthz/1, #{methods => [get]}},
                {"/readyz", fun {{name}}_health_controller:readyz/1, #{methods => [get]}}
            ]
        }
    ].
