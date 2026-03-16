-module({{name}}_router).
-behaviour(nova_router).

-export([
    routes/1
]).

routes(_Environment) ->
    [
        #{
            prefix => "",
            security => false,
            plugins => [
                {pre_request, otel_nova_plugin, #{}}
            ],
            routes => [
                {"/", fun {{name}}_main_controller:index/1, #{methods => [get]}},
                {"/health", fun {{name}}_health_controller:check/1, #{methods => [get]}},
                {"/health/ready", fun {{name}}_health_controller:ready/1, #{methods => [get]}}
            ]
        }
    ].
