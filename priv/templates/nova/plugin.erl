-module({{name}}_plugin).
-behaviour(nova_plugin).

-export([
         pre_request/2,
         post_request/2,
         plugin_info/0
        ]).

%%--------------------------------------------------------------------
%% @doc
%% Pre-request callback
%% @end
%%--------------------------------------------------------------------
-spec pre_request(Req :: cowboy_req:req(), Options :: map()) ->
                         {ok, Req0 :: cowboy_req:req()} |
                         {stop, Req0 :: cowboy_req:req()} |
                         {error, Reason :: term()}.
pre_request(Req, _Options) ->
    {ok, Req}.


%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Options :: map()) ->
                          {ok, Req0 :: cowboy_req:req()} |
                          {stop, Req0 :: cowboy_req:req()} |
                          {error, Reason :: term()}.
post_request(Req, _Options) ->
    {ok, Req}.


%%--------------------------------------------------------------------
%% @doc
%% nova_plugin callback. Returns information about the plugin.
%% @end
%%--------------------------------------------------------------------
-spec plugin_info() -> {Title :: binary(), Version :: binary(), Author :: binary(), Description :: binary(),
                       [{Key :: atom(), OptionDescription :: atom()}]}.
plugin_info() ->
    {<<"{{pluginname}} plugin">>,
     <<"0.0.1">>,
     <<"User <user@email.com">>,
     <<"Descriptive text">>,
     []}. %% Options is specified as {Key, Description}
