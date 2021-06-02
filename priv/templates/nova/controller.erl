-module({{name}}_main_controller).
-export([
         index/1
        ]).

-include_lib("nova/include/nova.hrl").

index(#{req := #{method := <<"GET">>}} = _NovaReq) ->
    {ok, [{message, "Hello world!"}]}.
