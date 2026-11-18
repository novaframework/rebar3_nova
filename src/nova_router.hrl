%% Nova's routing values, as Nova 1.0 defines them. A table compiled by an
%% earlier Nova has them without `http_versions', and the second under the
%% name `cowboy_handler_value': rebar3_nova_dispatch:payload/1 brings those
%% to this shape.
-record(nova_handler_value, {
    app :: atom(),
    module :: atom(),
    function :: atom(),
    callback :: function() | undefined,
    plugins = [] :: list(),
    secure = false :: false | {Mod :: atom(), Fun :: atom()},
    extra_state :: any(),
    http_versions :: undefined | [http1 | http2 | http3]
}).

-record(nova_module_value, {
    app :: atom(),
    handler :: atom(),
    arguments :: any(),
    plugins = [] :: list(),
    secure = false :: false | {Mod :: atom(), Fun :: atom()},
    http_versions :: undefined | [http1 | http2 | http3]
}).
