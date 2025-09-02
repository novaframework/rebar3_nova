-module(rebar3_nova).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    case parse_version(Vsn) of
        [Major, Minor| _Patch] when Major >= "3" andalso
                                   Minor > "15" ->
            lists:foldl(fun provider_init/2, {ok, State}, [rebar3_nova_prv, rebar3_nova_serve, rebar3_nova_routes]);
        ["git"] ->
            rebar_api:info("Compiling with rebar3 from git - make sure you know what you are doing"),
            lists:foldl(fun provider_init/2, {ok, State}, [rebar3_nova_prv, rebar3_nova_serve, rebar3_nova_routes]);
        SomethingElse ->
            rebar_api:abort("Nova needs Rebar > 3.15 to function. Your version is: ~p. Please consider upgrading.", [SomethingElse])
    end.

provider_init(Module, {ok, State}) ->
    Module:init(State).


parse_version(Vsn) ->
    parse_version(Vsn, "", []).

parse_version([], Ack, List) ->
    lists:reverse([lists:reverse(Ack)|List]);
parse_version([$.|Tl], Ack, List) ->
    parse_version(Tl, "", [lists:reverse(Ack)|List]);
parse_version([Char|Tl], Ack, List) ->
    parse_version(Tl, [Char|Ack], List).
