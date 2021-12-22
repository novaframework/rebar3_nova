-module({{name}}_main_controller).
-export([
         index/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.
