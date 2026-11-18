-module({{name}}_ws_controller).
-behaviour(nova_websocket).

-export([
    init/1,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-doc "Runs for the request that asks for the WebSocket, before it is upgraded.".
-spec init(State :: map()) -> {ok, State0 :: map()} | any().
init(State) ->
    {ok, State}.

-doc "Runs once the connection is a WebSocket.".
-spec websocket_init(State :: map()) -> nova_websocket:call_result().
websocket_init(State) ->
    {ok, State}.

-doc "Runs for every frame the client sends.".
-spec websocket_handle(Frame :: nova_websocket:in_frame(), State :: map()) ->
    nova_websocket:call_result().
websocket_handle(_Frame, State) ->
    {ok, State}.

-doc "Runs for every Erlang message the connection process receives.".
-spec websocket_info(Info :: any(), State :: map()) -> nova_websocket:call_result().
websocket_info(_Info, State) ->
    {ok, State}.

-doc "Runs as the connection closes.".
-spec terminate(Reason :: nova_websocket:reason(), PartialReq :: map(), State :: map()) -> ok.
terminate(_Reason, _PartialReq, _State) ->
    ok.
