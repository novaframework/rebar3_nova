-module({{name}}_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Environment) ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                %% Arizona LiveView routes
                {"/", {{name}}_home_view, #{protocol => liveview}},

                %% API routes
                {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},

                %% Static assets
                {"/assets/[...]", "static/assets"}
            ]
        }
    ].
