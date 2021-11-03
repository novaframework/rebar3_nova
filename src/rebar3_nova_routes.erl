-module(rebar3_nova_routes).

-export([init/1, do/1, format_error/1]).

-include("nova_router.hrl").
-include_lib("routing_tree/include/routing_tree.hrl").

-define(PROVIDER, routes).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, nova},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 nova routes list"}, % How to use the plugin
            {opts, [{list, undefined, "list", string, "List all routes"}]},
            {short_desc, "Nova route plugin"},
            {desc, "Plugin to handle nova routes"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [Hd|_] = rebar_state:project_apps(State),
    App = erlang:binary_to_atom(rebar_app_info:name(Hd)),
    Dispatch = nova_router:compile([App]),
    print_routes(Dispatch),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%% ===================================================================
%% Private functions
%% ===================================================================
print_routes(Dispatch) ->
    format_tree(Dispatch).

format_tree([]) -> ok;
format_tree(#host_tree{hosts = Hosts}) ->
    format_tree(Hosts);
format_tree([{Host, #routing_tree{tree = Tree}}|Tl]) ->
    io:format("Host: ~p~n", [Host]),
    format_tree(Tree, 1) ++ format_tree(Tl).

format_tree([], _Depth) -> [];
format_tree([#node{segment = Segment, value = [], children = Children}|Tl], Depth) ->
    %% Just a plain node
    Segment0 =
        case false of
            _ when is_list(Segment) orelse
                   is_binary(Segment) ->
                Segment;
            _ when is_integer(Segment) ->
                erlang:integer_to_list(Segment);
            E ->
                "[...]"
        end,
    Prefix = [ $  || _X <- lists:seq(0, Depth*4) ],
    case Tl of
        [] ->
            io:format("~ts~ts /~ts~n", [Prefix, <<226,148,148,226,148,128,32>>, Segment0]);
        _ ->
            io:format("~ts~ts /~ts~n", [Prefix, <<226,148,156,226,148,128,32>>, Segment0])
    end,
    format_tree(Children, Depth+1),
    format_tree(Tl, Depth);

format_tree([#node{segment = Segment, value = Value, children = Children}|Tl], Depth) ->
    Segment0 =
        case false of
            _ when is_list(Segment) orelse
                   is_binary(Segment) ->
                Segment;
            _ when is_integer(Segment) ->
                erlang:integer_to_list(Segment);
            E ->
                "[...]"
        end,
    Prefix = [ $  || _X <- lists:seq(0, Depth*4) ],

    lists:foreach(fun(#node_comp{comparator = Method, value = Value}) ->
                          {App, Mod, Func} = case Value of
                                                 #nova_handler_value{app = App0, module = Mod0, function = Func0} -> {App0, Mod0, Func0};
                                                 #cowboy_handler_value{app = App0, handler = Handler} -> {App0, Handler, init}
                                             end,
                          case Tl of
                              [] ->
                                  io:format("~ts~ts ~ts /~ts (~ts, ~ts:~ts/1)~n", [Prefix, <<226,148,148,226,148,128,32>>, Method, Segment0, App, Mod, Func]);
                              _ ->
                                  io:format("~ts~ts ~ts /~ts (~ts, ~ts:~ts/1)~n", [Prefix, <<226,148,156,226,148,128,32>>, Method, Segment0, App, Mod, Func])
                              end
                  end, Value),
    format_tree(Children, Depth+1),
    format_tree(Tl, Depth).
