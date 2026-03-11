-module({{name}}_health_controller).
-export([
    healthz/1,
    readyz/1
]).

healthz(_Req) ->
    {json, #{status => <<"ok">>}}.

readyz(_Req) ->
    ready_response(check_ready()).

ready_response(true) ->
    {json, #{status => <<"ready">>}};
ready_response(false) ->
    {status, 503, #{}, #{status => <<"unavailable">>}}.

check_ready() ->
    Apps = application:which_applications(),
    lists:keymember({{name}}, 1, Apps).
