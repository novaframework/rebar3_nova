-module(rebar3_nova).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_nova_prv:init(State),
    {ok, State1}.
