-module(rebar3_nova).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    check_new_version(),
    {ok, Vsn} = application:get_key(rebar, vsn),
    case parse_version(Vsn) of
        [Major, Minor, _Patch] when Major >= "3" andalso
                                   Minor > "15" ->
            lists:foldl(fun provider_init/2, {ok, State}, [rebar3_nova_prv, rebar3_nova_serve, rebar3_nova_routes]);
        _ ->
            rebar_api:abort("Vsn: ~p", [Vsn])
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



check_new_version() ->
    {ok, {_, _, Body}} = httpc:request(get, {"https://hex.pm/api/packages/rebar3_nova", [{"accept", "application/json"}, {"user-agent", "nova"}]}, [], []),
    {ok, #{<<"latest_version">> := LatestVersion}} = thoas:decode(Body),
    {ok, Version} = application:get_key(rebar3_nova, vsn),
    case ec_semver:gt(LatestVersion, Version) of
        true ->
            %% There's a newer version of the plugin out there
            rebar_log:log(warn, "There's a newer version of rebar3_nova released. Please consider upgrading it! Your version is ~s and the latest released version is ~s.~n~nTo upgrade the plugin just run: rebar3 as global plugins upgrade rebar3_nova", [Version, LatestVersion]);
        _ ->
            ok
    end.
