-module({{name}}).
-behaviour(nova_plugin).

-export([
    pre_security/4,
    pre_request/4,
    post_request/4,
    plugin_info/0
]).

-doc """
Runs ahead of the security callback of the route, for every request to it,
on a Nova later than 0.18. Nova 0.18 and earlier do not call it. It must not
read the request body.
""".
-spec pre_security(Req :: nova_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: nova_req:req(), NewState :: any()}
    | {stop, Req0 :: nova_req:req(), NewState :: any()}
    | {error, Reason :: term()}.
pre_security(Req, _Env, _Options, State) ->
    {ok, Req, State}.

-doc """
Runs before the controller. Up to Nova 0.18 that is ahead of the security
callback of the route, on a later Nova once the callback has let the request
through.
""".
-spec pre_request(Req :: nova_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: nova_req:req(), NewState :: any()}
    | {stop, Req0 :: nova_req:req(), NewState :: any()}
    | {error, Reason :: term()}.
pre_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

-doc "Runs after the controller has answered.".
-spec post_request(Req :: nova_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: nova_req:req(), NewState :: any()}
    | {stop, Req0 :: nova_req:req(), NewState :: any()}
    | {error, Reason :: term()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

-spec plugin_info() ->
    #{
        title := binary(),
        version := binary(),
        url := binary(),
        authors := [binary()],
        description := binary(),
        options := [{Key :: atom(), Description :: binary()}]
    }.
plugin_info() ->
    #{
        title => <<"{{name}}">>,
        version => <<"0.0.1">>,
        url => <<"https://example.com">>,
        authors => [<<"User <user@email.com>">>],
        description => <<"Descriptive text">>,
        options => []
    }.
