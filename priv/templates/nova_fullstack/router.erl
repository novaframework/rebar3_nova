-module({{name}}_router).
-behaviour(nova_router).

-export([routes/1]).

%% Live pages are declared in the arizona_nova env in config/*_sys.config.src.
routes(_Environment) ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                {"/assets/[...]", "static/assets"}
            ]
        }
    ].
