-module({{name}}_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = {{name}}_sup:start_link(),
    ok = kura:start({{name}}_repo),
    {ok, Pid}.

stop(_State) ->
    ok.
