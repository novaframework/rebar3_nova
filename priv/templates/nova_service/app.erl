-module({{name}}_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    opentelemetry_nova:setup(#{prometheus => #{port => 9464}}),
    {{name}}_sup:start_link().

stop(_State) ->
    ok.
