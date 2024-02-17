-module({{name}}).

-export([is_authorized/1]).

-include_lib("oidcc/include/oidcc_token_introspection.hrl").

is_authorized(Req) ->
    case cowboy_req:headers(Req) of
        #{<<"authorization">> := <<"Bearer ", Token/binary>>} ->
           case nova_oidcc:load_user_info(maps:put(oidcc_extract_authorization, Token, Req)) of
               {error, inactive_token, OidccReq} -> send_inactive_token_response(OidccReq);
               {ok, #{oidcc_validate_jwt_token := Claims}} -> {true, Claims}
            end;
        #{<<"authorization">> := Authorization} ->
            send_invalid_header_response(Req, Authorization);
        #{} ->
            false
    end.

send_inactive_token_response(Req0) ->
    Req = cowboy_req:reply(
        401,
        #{<<"content-type">> => <<"text/plain">>},
        <<"The provided token is inactive">>,
        Req0
    ),
    {stop, Req}.

send_invalid_header_response(Req0, GivenHeader) ->
    Req = cowboy_req:reply(
        400,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Invalid authorization Header\n\nExpected: Authorization: Bearer <token>\nGiven: ",
            GivenHeader/binary>>,
        Req0
    ),
    {stop, Req}.