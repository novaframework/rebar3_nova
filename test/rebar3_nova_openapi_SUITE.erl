-module(rebar3_nova_openapi_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    build_spec_minimal/1,
    build_spec_with_schemas/1,
    build_paths_groups_by_path/1,
    extract_path_params_none/1,
    extract_path_params_single/1,
    extract_path_params_multiple/1,
    schema_ref_with_json_schema/1,
    schema_ref_without/1,
    maybe_add_request_body_post/1,
    maybe_add_request_body_get/1,
    maybe_add_response_schema_with_ref/1,
    maybe_add_response_schema_without/1,
    segment_to_binary_test/1,
    method_to_binary_test/1,
    swagger_html_contains_script/1
]).

all() ->
    [
        build_spec_minimal,
        build_spec_with_schemas,
        build_paths_groups_by_path,
        extract_path_params_none,
        extract_path_params_single,
        extract_path_params_multiple,
        schema_ref_with_json_schema,
        schema_ref_without,
        maybe_add_request_body_post,
        maybe_add_request_body_get,
        maybe_add_response_schema_with_ref,
        maybe_add_response_schema_without,
        segment_to_binary_test,
        method_to_binary_test,
        swagger_html_contains_script
    ].

build_spec_minimal(_Config) ->
    Spec = rebar3_nova_openapi:build_spec("My API", "1.0.0", [], #{}),
    ?assertEqual(<<"3.0.3">>, maps:get(<<"openapi">>, Spec)),
    Info = maps:get(<<"info">>, Spec),
    ?assertEqual(<<"My API">>, maps:get(<<"title">>, Info)),
    ?assertEqual(<<"1.0.0">>, maps:get(<<"version">>, Info)),
    ?assertEqual(#{}, maps:get(<<"paths">>, Spec)),
    ?assertNot(maps:is_key(<<"components">>, Spec)).

build_spec_with_schemas(_Config) ->
    Schemas = #{<<"user">> => #{<<"type">> => <<"object">>}},
    Spec = rebar3_nova_openapi:build_spec("API", "0.1.0", [], Schemas),
    ?assert(maps:is_key(<<"components">>, Spec)),
    Components = maps:get(<<"components">>, Spec),
    ?assertEqual(Schemas, maps:get(<<"schemas">>, Components)).

build_paths_groups_by_path(_Config) ->
    Routes = [
        {<<"/users">>, <<"get">>, myapp_users, list, #{}},
        {<<"/users">>, <<"post">>, myapp_users, create, #{}},
        {<<"/users/{id}">>, <<"get">>, myapp_users, show, #{}}
    ],
    Paths = rebar3_nova_openapi:build_paths(Routes),
    ?assert(maps:is_key(<<"/users">>, Paths)),
    ?assert(maps:is_key(<<"/users/{id}">>, Paths)),
    UsersMethods = maps:get(<<"/users">>, Paths),
    ?assert(maps:is_key(<<"get">>, UsersMethods)),
    ?assert(maps:is_key(<<"post">>, UsersMethods)),
    GetOp = maps:get(<<"get">>, UsersMethods),
    ?assertEqual(<<"myapp_users.list">>, maps:get(<<"operationId">>, GetOp)).

extract_path_params_none(_Config) ->
    ?assertEqual([], rebar3_nova_openapi:extract_path_params(<<"/users">>)).

extract_path_params_single(_Config) ->
    Params = rebar3_nova_openapi:extract_path_params(<<"/users/{id}">>),
    ?assertEqual(1, length(Params)),
    [P] = Params,
    ?assertEqual(<<"id">>, maps:get(<<"name">>, P)),
    ?assertEqual(<<"path">>, maps:get(<<"in">>, P)),
    ?assertEqual(true, maps:get(<<"required">>, P)).

extract_path_params_multiple(_Config) ->
    Params = rebar3_nova_openapi:extract_path_params(<<"/orgs/{org_id}/users/{user_id}">>),
    ?assertEqual(2, length(Params)),
    Names = [maps:get(<<"name">>, P) || P <- Params],
    ?assert(lists:member(<<"org_id">>, Names)),
    ?assert(lists:member(<<"user_id">>, Names)).

schema_ref_with_json_schema(_Config) ->
    Extra = #{json_schema => "priv/schemas/user.json"},
    {ok, Ref} = rebar3_nova_openapi:schema_ref(Extra),
    ?assertEqual(<<"#/components/schemas/user">>, Ref).

schema_ref_without(_Config) ->
    ?assertEqual(none, rebar3_nova_openapi:schema_ref(#{})),
    ?assertEqual(none, rebar3_nova_openapi:schema_ref(undefined)).

maybe_add_request_body_post(_Config) ->
    Ref = {ok, <<"#/components/schemas/user">>},
    Op = #{<<"operationId">> => <<"test">>},
    Result = rebar3_nova_openapi:maybe_add_request_body(<<"post">>, Ref, Op),
    ?assert(maps:is_key(<<"requestBody">>, Result)),
    Body = maps:get(<<"requestBody">>, Result),
    ?assertEqual(true, maps:get(<<"required">>, Body)).

maybe_add_request_body_get(_Config) ->
    Ref = {ok, <<"#/components/schemas/user">>},
    Op = #{<<"operationId">> => <<"test">>},
    Result = rebar3_nova_openapi:maybe_add_request_body(<<"get">>, Ref, Op),
    ?assertNot(maps:is_key(<<"requestBody">>, Result)).

maybe_add_response_schema_with_ref(_Config) ->
    Ref = {ok, <<"#/components/schemas/user">>},
    Op = #{<<"operationId">> => <<"test">>},
    Result = rebar3_nova_openapi:maybe_add_response_schema(<<"get">>, Ref, Op),
    Responses = maps:get(<<"responses">>, Result),
    R200 = maps:get(<<"200">>, Responses),
    ?assert(maps:is_key(<<"content">>, R200)).

maybe_add_response_schema_without(_Config) ->
    Op = #{<<"operationId">> => <<"test">>},
    Result = rebar3_nova_openapi:maybe_add_response_schema(<<"get">>, none, Op),
    Responses = maps:get(<<"responses">>, Result),
    R200 = maps:get(<<"200">>, Responses),
    ?assertNot(maps:is_key(<<"content">>, R200)),
    ?assertEqual(<<"Successful response">>, maps:get(<<"description">>, R200)).

segment_to_binary_test(_Config) ->
    ?assertEqual(<<"users">>, rebar3_nova_openapi:segment_to_binary(<<"users">>, false)),
    ?assertEqual(<<"{id}">>, rebar3_nova_openapi:segment_to_binary(<<"id">>, true)),
    ?assertEqual(<<"posts">>, rebar3_nova_openapi:segment_to_binary("posts", false)).

method_to_binary_test(_Config) ->
    ?assertEqual(<<"get">>, rebar3_nova_openapi:method_to_binary(get)),
    ?assertEqual(<<"post">>, rebar3_nova_openapi:method_to_binary(post)),
    ?assertEqual(<<"get">>, rebar3_nova_openapi:method_to_binary(<<"GET">>)).

swagger_html_contains_script(_Config) ->
    Html = lists:flatten(rebar3_nova_openapi:swagger_html("openapi.json")),
    ?assertNotEqual(nomatch, string:find(Html, "swagger-ui")),
    ?assertNotEqual(nomatch, string:find(Html, "openapi.json")),
    ?assertNotEqual(nomatch, string:find(Html, "SwaggerUIBundle")).
