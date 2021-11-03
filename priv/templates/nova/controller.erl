-module({{name}}_main_controller).
-export([
         index/1
        ]).

-include_lib("nova/include/nova.hrl").

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.
