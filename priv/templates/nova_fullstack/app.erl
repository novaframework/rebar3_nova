-module({{name}}_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = kura_migrator:migrate({{name}}_repo),
    {{name}}_sup:start_link().

stop(_State) ->
    ok.
