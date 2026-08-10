%%% Covers reading a compiled Nova dispatch table.
%%%
%%% The route-reporting tasks used to walk routing_tree's records directly.
%%% They now go through rebar3_nova_dispatch, and this suite builds a real
%%% dispatch table with nova_router and checks each task can still read it.
-module(rebar3_nova_dispatch_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("nova/include/nova_router.hrl").

all() ->
    [
        routes_are_flattened,
        status_code_routes_are_left_out,
        openapi_paths_use_braces,
        catch_all_is_dropped_from_openapi_paths,
        methods_are_lowercased,
        audit_reads_the_table,
        openapi_reads_the_table,
        doctor_reads_the_table,
        routes_task_prints_the_table
    ].

init_per_suite(Config) ->
    application:load(nova),
    application:set_env(nova, dispatch_backend, persistent_term),
    [{dispatch, dispatch()} | Config].

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% A dispatch table covering the shapes the tasks care about
%%====================================================================

dispatch() ->
    Value = #nova_handler_value{
        app = test_app,
        callback = fun test_controller:index/1,
        secure = false,
        plugins = []
    },
    Trie0 = nova_routing_trie:new(#{}),
    {ok, Trie1} = nova_routing_trie:insert('_', "/users", <<"GET">>, Value, Trie0),
    {ok, Trie2} = nova_routing_trie:insert('_', "/users", <<"POST">>, Value, Trie1),
    {ok, Trie3} = nova_routing_trie:insert('_', "/users/:id", <<"GET">>, Value, Trie2),
    {ok, Trie4} = nova_routing_trie:insert(
        '_',
        "/assets/[...]",
        '_',
        Value#nova_handler_value{module = nova_file_controller},
        Trie3
    ),
    {ok, Trie5} = nova_routing_trie:insert(
        '_',
        "/ws",
        '_',
        #cowboy_handler_value{
            app = test_app,
            handler = nova_ws_handler,
            arguments = #{},
            plugins = [],
            secure = false
        },
        Trie4
    ),
    {ok, Trie6} = nova_routing_trie:insert(
        '_',
        404,
        '_',
        Value#nova_handler_value{module = nova_error_controller},
        Trie5
    ),
    Trie6.

%%====================================================================
%% rebar3_nova_dispatch
%%====================================================================

routes_are_flattened(Config) ->
    Routes = rebar3_nova_dispatch:routes(?config(dispatch, Config)),
    Paths = lists:usort([Path || {Path, _Method, _Payload} <- Routes]),
    ?assertEqual([<<"/assets/[...]">>, <<"/users">>, <<"/users/:id">>, <<"/ws">>], Paths),
    %% /users carries two methods, so it appears twice.
    ?assertEqual(2, length([P || {P, _M, _V} <- Routes, P =:= <<"/users">>])).

status_code_routes_are_left_out(Config) ->
    Routes = rebar3_nova_dispatch:routes(?config(dispatch, Config)),
    ?assertEqual([], [R || R = {Path, _M, _V} <- Routes, not is_binary(Path)]).

openapi_paths_use_braces(_Config) ->
    ?assertEqual(<<"/users/{id}">>, rebar3_nova_dispatch:openapi_path(<<"/users/:id">>)),
    ?assertEqual(<<"/users">>, rebar3_nova_dispatch:openapi_path(<<"/users">>)),
    ?assertEqual(<<"/a/{b}/c/{d}">>, rebar3_nova_dispatch:openapi_path(<<"/a/:b/c/:d">>)),
    ?assertEqual(<<"/">>, rebar3_nova_dispatch:openapi_path(<<"/">>)).

catch_all_is_dropped_from_openapi_paths(_Config) ->
    ?assertEqual(<<"/assets">>, rebar3_nova_dispatch:openapi_path(<<"/assets/[...]">>)),
    ?assertEqual(<<"/">>, rebar3_nova_dispatch:openapi_path(<<"/[...]">>)).

methods_are_lowercased(_Config) ->
    ?assertEqual(<<"get">>, rebar3_nova_dispatch:method_to_binary(<<"GET">>)),
    ?assertEqual(<<"any">>, rebar3_nova_dispatch:method_to_binary('_')),
    ?assertEqual(<<"post">>, rebar3_nova_dispatch:method_to_binary(post)).

%%====================================================================
%% The tasks
%%====================================================================

%% The audit skips file and error controllers and websocket handlers, and
%% expands an any-method route across the verbs it reports on.
audit_reads_the_table(Config) ->
    Routes = rebar3_nova_audit:collect_routes(?config(dispatch, Config)),
    Paths = lists:usort([Path || {Path, _M, _S, _Mod, _Any} <- Routes]),
    ?assertEqual([<<"/users">>, <<"/users/{id}">>], Paths),
    ?assertEqual(
        [<<"get">>, <<"post">>],
        lists:usort([M || {<<"/users">>, M, _S, _Mod, _Any} <- Routes])
    ).

openapi_reads_the_table(Config) ->
    Routes = rebar3_nova_openapi:collect_routes(?config(dispatch, Config)),
    Paths = lists:usort([Path || {Path, _M, _Mod, _F, _E} <- Routes]),
    ?assertEqual([<<"/users">>, <<"/users/{id}">>], Paths),
    ?assert(lists:all(fun({_P, _M, Mod, _F, _E}) -> Mod =:= test_controller end, Routes)).

%% The doctor reports on every route, websockets included.
doctor_reads_the_table(Config) ->
    Handlers = rebar3_nova_doctor:collect_route_handlers(?config(dispatch, Config)),
    ?assertEqual(5, length(Handlers)),
    ?assertMatch(
        [{<<"/ws">>, '_', nova_ws_handler, init, 2}],
        [H || H = {<<"/ws">>, _M, _Mod, _F, _A} <- Handlers]
    ),
    ?assert(lists:all(fun({_P, _M, Mod, _F, _A}) -> Mod =/= unknown end, Handlers)).

routes_task_prints_the_table(Config) ->
    Output = capture(fun() -> rebar3_nova_routes:print_routes(?config(dispatch, Config)) end),
    ?assertNotEqual(nomatch, string:find(Output, "/users/:id")),
    ?assertNotEqual(nomatch, string:find(Output, "test_controller:index/1")),
    ?assertNotEqual(nomatch, string:find(Output, "(status 404)")),
    ?assertNotEqual(nomatch, string:find(Output, "nova_ws_handler")).

%%====================================================================
%% Helpers
%%====================================================================

capture(Fun) ->
    Self = self(),
    Ref = make_ref(),
    {Pid, MonRef} =
        spawn_monitor(fun() ->
            group_leader(spawn_capture(Self, Ref), self()),
            Fun(),
            exit(normal)
        end),
    receive
        {'DOWN', MonRef, process, Pid, _Reason} -> ok
    after 5000 ->
        ct:fail(capture_timeout)
    end,
    collect_output(Ref, []).

spawn_capture(Owner, Ref) ->
    spawn(fun() -> capture_loop(Owner, Ref) end).

capture_loop(Owner, Ref) ->
    receive
        {io_request, From, ReplyAs, {put_chars, unicode, Chars}} ->
            Owner ! {Ref, unicode:characters_to_list(Chars)},
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Owner, Ref);
        {io_request, From, ReplyAs, {put_chars, unicode, M, F, A}} ->
            Owner ! {Ref, unicode:characters_to_list(apply(M, F, A))},
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Owner, Ref);
        {io_request, From, ReplyAs, _Other} ->
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Owner, Ref)
    end.

collect_output(Ref, Acc) ->
    receive
        {Ref, Chars} -> collect_output(Ref, [Chars | Acc])
    after 0 ->
        lists:flatten(lists:reverse(Acc))
    end.
