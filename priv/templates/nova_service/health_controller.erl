-module({{name}}_health_controller).
-export([
    check/1,
    ready/1
]).

%% Liveness check — always returns 200 if the application is running
check(_Req) ->
    {json, 200, #{}, #{status => <<"ok">>}}.

%% Readiness check — verifies database and Kafka connectivity
ready(_Req) ->
    Checks = #{
        database => check_database(),
        kafka => check_kafka()
    },
    AllHealthy = maps:fold(fun(_, V, Acc) -> Acc andalso V =:= <<"ok">> end, true, Checks),
    Status = case AllHealthy of
        true -> 200;
        false -> 503
    end,
    {json, Status, #{}, Checks#{status => status_text(AllHealthy)}}.

check_database() ->
    try
        case pgo:query("SELECT 1") of
            #{command := select} -> <<"ok">>;
            _ -> <<"error">>
        end
    catch
        _:_ -> <<"error">>
    end.

check_kafka() ->
    try
        case brod:get_metadata(kafka_client, []) of
            {ok, _} -> <<"ok">>;
            _ -> <<"error">>
        end
    catch
        _:_ -> <<"error">>
    end.

status_text(true) -> <<"ok">>;
status_text(false) -> <<"degraded">>.
