-module(rebar3_nova).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    lists:foldl(fun provider_init/2, {ok, State}, [rebar3_nova_prv, rebar3_nova_serve, rebar3_nova_routes]).

provider_init(Module, {ok, State}) ->
    Module:init(State).
