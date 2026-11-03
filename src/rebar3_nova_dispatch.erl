%%%-------------------------------------------------------------------
%%% @doc
%%% Reads a compiled Nova dispatch table.
%%%
%%% The routing table used to be a `routing_tree' record that every task
%%% walked for itself. It is now an opaque structure owned by
%%% `nova_routing_trie', so this module is the single place that talks to its
%%% introspection API and hands the tasks a flat list of routes.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_nova_dispatch).

-export([
    routes/1,
    handler/1,
    openapi_path/1,
    method_to_binary/1
]).

-include("nova_router.hrl").

-type route() :: {Path :: binary(), Method :: '_' | binary(), Payload :: term()}.
-type handler() ::
    {controller, module(), atom()}
    | {anonymous, module()}
    | {cowboy, module()}
    | static
    | error_page
    | unknown.
-export_type([route/0, handler/0]).

%%--------------------------------------------------------------------
%% @doc
%% Every URL route in a compiled dispatch table.
%%
%% Status-code routes (Nova's error pages, which are keyed by integer rather
%% than by path) are left out, since none of the tasks that call this report
%% on them.
%% @end
%%--------------------------------------------------------------------
-spec routes(Dispatch :: nova_routing_trie:trie()) -> [route()].
routes(Dispatch) ->
    [
        {Path, Method, Payload}
     || {_Host, Path, Method, Payload} <- nova_routing_trie:routes(Dispatch),
        is_binary(Path)
    ].

%%--------------------------------------------------------------------
%% @doc
%% What serves a route. Nova 0.18 stores every handler as a callback fun,
%% static files and error pages included, so the module has to be read
%% from the fun rather than from the record's `module' field.
%% @end
%%--------------------------------------------------------------------
-spec handler(Payload :: term()) -> handler().
handler(#nova_handler_value{callback = Callback}) when is_function(Callback) ->
    fun_handler(Callback);
handler(#nova_handler_value{module = Module, function = Function}) when
    is_atom(Module), Module =/= undefined, is_atom(Function)
->
    module_handler(Module, Function);
handler(#cowboy_handler_value{handler = Handler}) ->
    {cowboy, Handler};
handler(_) ->
    unknown.

fun_handler(Fun) ->
    {module, Module} = erlang:fun_info(Fun, module),
    {name, Name} = erlang:fun_info(Fun, name),
    case erlang:fun_info(Fun, type) of
        {type, external} -> module_handler(Module, Name);
        {type, local} -> {anonymous, Module}
    end.

module_handler(nova_file_controller, _Function) -> static;
module_handler(nova_error_controller, _Function) -> error_page;
module_handler(Module, Function) -> {controller, Module, Function}.

%%--------------------------------------------------------------------
%% @doc
%% Rewrite a Nova path into the OpenAPI style, so `/users/:id' becomes
%% `/users/{id}'. A trailing `[...]' catch-all has no OpenAPI equivalent and
%% is dropped.
%% @end
%%--------------------------------------------------------------------
-spec openapi_path(Path :: binary()) -> binary().
openapi_path(Path) ->
    Segments = [openapi_segment(S) || S <- binary:split(Path, <<"/">>, [global]), S =/= <<>>],
    case [S || S <- Segments, S =/= skip] of
        [] -> <<"/">>;
        Kept -> <<<<"/", S/binary>> || S <- Kept>>
    end.

openapi_segment(<<":", Name/binary>>) -> <<"{", Name/binary, "}">>;
openapi_segment(<<"[...]">>) -> skip;
openapi_segment(Segment) -> Segment.

%%--------------------------------------------------------------------
%% @doc
%% The lowercase name of an HTTP method, as the report formats want it.
%% @end
%%--------------------------------------------------------------------
-spec method_to_binary(Method :: '_' | binary() | atom()) -> binary().
method_to_binary('_') -> <<"any">>;
method_to_binary(Method) when is_binary(Method) -> string:lowercase(Method);
method_to_binary(Method) when is_atom(Method) -> string:lowercase(atom_to_binary(Method, utf8)).
