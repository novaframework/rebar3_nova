-module({{name}}_main_controller).
-export([
         index/1
        ]).

index(_Req) ->
    {{#use_bossdb}}
    Timestamp = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    Model = {{name}}_model:new(id, Timestamp),
    Model:save(),
    {{/use_bossdb}}
    {ok, [{message, "Hello world!"}]}.
