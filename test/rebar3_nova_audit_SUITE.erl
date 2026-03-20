-module(rebar3_nova_audit_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    is_mutation_test/1,
    classify_findings_mutation_warning/1,
    classify_findings_wildcard_warning/1,
    classify_findings_get_info/1,
    classify_findings_secure_no_warning/1,
    segment_to_binary_test/1,
    method_to_binary_test/1,
    expand_methods_wildcard/1,
    expand_methods_single/1
]).

all() ->
    [
        is_mutation_test,
        classify_findings_mutation_warning,
        classify_findings_wildcard_warning,
        classify_findings_get_info,
        classify_findings_secure_no_warning,
        segment_to_binary_test,
        method_to_binary_test,
        expand_methods_wildcard,
        expand_methods_single
    ].

is_mutation_test(_Config) ->
    ?assert(rebar3_nova_audit:is_mutation(<<"post">>)),
    ?assert(rebar3_nova_audit:is_mutation(<<"put">>)),
    ?assert(rebar3_nova_audit:is_mutation(<<"delete">>)),
    ?assert(rebar3_nova_audit:is_mutation(<<"patch">>)),
    ?assertNot(rebar3_nova_audit:is_mutation(<<"get">>)),
    ?assertNot(rebar3_nova_audit:is_mutation(<<"head">>)),
    ?assertNot(rebar3_nova_audit:is_mutation(<<"options">>)).

classify_findings_mutation_warning(_Config) ->
    Routes = [{<<"/users">>, <<"post">>, false, myapp_controller, false}],
    {Warnings, _Infos} = rebar3_nova_audit:classify_findings(Routes),
    ?assertEqual(1, length(Warnings)),
    [W] = Warnings,
    ?assertNotEqual(nomatch, string:find(lists:flatten(W), "POST")).

classify_findings_wildcard_warning(_Config) ->
    Routes = [{<<"/users">>, <<"get">>, false, myapp_controller, true}],
    {Warnings, _Infos} = rebar3_nova_audit:classify_findings(Routes),
    ?assertEqual(1, length(Warnings)),
    [W] = Warnings,
    ?assertNotEqual(nomatch, string:find(lists:flatten(W), "Wildcard")).

classify_findings_get_info(_Config) ->
    Routes = [{<<"/users">>, <<"get">>, false, myapp_controller, false}],
    {Warnings, Infos} = rebar3_nova_audit:classify_findings(Routes),
    ?assertEqual(0, length(Warnings)),
    ?assertEqual(1, length(Infos)),
    [I] = Infos,
    ?assertNotEqual(nomatch, string:find(lists:flatten(I), "GET")).

classify_findings_secure_no_warning(_Config) ->
    Routes = [
        {<<"/users">>, <<"post">>, {myapp_auth, check}, myapp_controller, false},
        {<<"/users">>, <<"get">>, {myapp_auth, check}, myapp_controller, false}
    ],
    {Warnings, Infos} = rebar3_nova_audit:classify_findings(Routes),
    ?assertEqual(0, length(Warnings)),
    ?assertEqual(0, length(Infos)).

segment_to_binary_test(_Config) ->
    ?assertEqual(<<"users">>, rebar3_nova_audit:segment_to_binary(<<"users">>, false)),
    ?assertEqual(<<"{id}">>, rebar3_nova_audit:segment_to_binary(<<"id">>, true)),
    ?assertEqual(<<"posts">>, rebar3_nova_audit:segment_to_binary("posts", false)),
    ?assertEqual(<<"{slug}">>, rebar3_nova_audit:segment_to_binary("slug", true)).

method_to_binary_test(_Config) ->
    ?assertEqual(<<"get">>, rebar3_nova_audit:method_to_binary(get)),
    ?assertEqual(<<"post">>, rebar3_nova_audit:method_to_binary(post)),
    ?assertEqual(<<"get">>, rebar3_nova_audit:method_to_binary(<<"GET">>)),
    ?assertEqual(<<"post">>, rebar3_nova_audit:method_to_binary(<<"POST">>)).

expand_methods_wildcard(_Config) ->
    {true, Routes} = rebar3_nova_audit:expand_methods('_', <<"/users">>, myapp, false),
    ?assertEqual(5, length(Routes)),
    Methods = [M || {_, M, _, _, _} <- Routes],
    ?assert(lists:member(<<"get">>, Methods)),
    ?assert(lists:member(<<"post">>, Methods)),
    ?assert(lists:member(<<"delete">>, Methods)),
    lists:foreach(
        fun({Path, _, _, Mod, IsWild}) ->
            ?assertEqual(<<"/users">>, Path),
            ?assertEqual(myapp, Mod),
            ?assert(IsWild)
        end,
        Routes
    ).

expand_methods_single(_Config) ->
    {true, Routes} = rebar3_nova_audit:expand_methods(get, <<"/users">>, myapp, false),
    ?assertEqual(1, length(Routes)),
    [{<<"/users">>, <<"get">>, false, myapp, false}] = Routes.
