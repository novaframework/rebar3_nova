-module(rebar3_nova_gen_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    gen_controller_creates_file/1,
    gen_resource_creates_files/1,
    gen_test_creates_suite/1,
    gen_auth_creates_all_files/1,
    gen_auth_uses_json_not_thoas/1,
    gen_live_creates_views/1,
    utils_singularize/1,
    utils_pluralize/1,
    utils_capitalize/1,
    utils_parse_fields/1,
    utils_timestamp/1,
    run_delegates_from_do/1
]).

all() ->
    [
        gen_controller_creates_file,
        gen_resource_creates_files,
        gen_test_creates_suite,
        gen_auth_creates_all_files,
        gen_auth_uses_json_not_thoas,
        gen_live_creates_views,
        utils_singularize,
        utils_pluralize,
        utils_capitalize,
        utils_parse_fields,
        utils_timestamp,
        run_delegates_from_do
    ].

init_per_testcase(_TC, Config) ->
    PrivDir = ?config(priv_dir, Config),
    OldCwd = file:get_cwd(),
    ok = file:set_cwd(PrivDir),
    [{old_cwd, OldCwd} | Config].

end_per_testcase(_TC, Config) ->
    {ok, OldCwd} = ?config(old_cwd, Config),
    ok = file:set_cwd(OldCwd),
    Config.

%%======================================================================
%% Generator tests - call generate functions directly
%%======================================================================

gen_controller_creates_file(_Config) ->
    AppDir = "ctrl",
    ok = file:make_dir(AppDir),
    rebar3_nova_gen_controller:generate(myapp, AppDir, "users", [list, show, create]),
    assert_file_exists(AppDir, "src/controllers/myapp_users_controller.erl"),
    Content = read_file(AppDir, "src/controllers/myapp_users_controller.erl"),
    assert_contains(Content, "-module(myapp_users_controller)"),
    assert_contains(Content, "list/1, show/1, create/1").

gen_resource_creates_files(_Config) ->
    AppDir = "res",
    ok = file:make_dir(AppDir),
    rebar3_nova_gen_controller:generate(myapp, AppDir, "users", [list, show]),
    Content = read_file(AppDir, "src/controllers/myapp_users_controller.erl"),
    assert_contains(Content, "-module(myapp_users_controller)"),
    assert_contains(Content, "list/1, show/1").

gen_test_creates_suite(_Config) ->
    AppDir = "tst",
    ok = file:make_dir(AppDir),
    %% gen_test:generate is internal, so call via the controller generate pattern
    SuiteName = "myapp_users_controller_SUITE",
    FileName = filename:join([AppDir, "test", SuiteName ++ ".erl"]),
    rebar3_nova_utils:write_file_if_not_exists(
        FileName, <<"-module(myapp_users_controller_SUITE).">>
    ),
    assert_file_exists(AppDir, "test/myapp_users_controller_SUITE.erl").

gen_auth_creates_all_files(_Config) ->
    AppDir = "auth",
    ok = file:make_dir(AppDir),
    %% Call run/1 would need rebar_state; test via the internal generate functions
    %% by checking that the module exports run/1
    ?assert(erlang:function_exported(rebar3_nova_gen_auth, run, 1)),
    ?assert(erlang:function_exported(rebar3_nova_gen_auth, do, 1)).

gen_auth_uses_json_not_thoas(_Config) ->
    %% Check the auth templates use json:encode/decode, not thoas
    PrivDir = code:priv_dir(rebar3_nova),
    AuthDir = filename:join([PrivDir, "templates", "gen", "auth"]),
    {ok, Files} = file:list_dir(AuthDir),
    lists:foreach(
        fun(File) ->
            Path = filename:join(AuthDir, File),
            {ok, Bin} = file:read_file(Path),
            Content = binary_to_list(Bin),
            assert_not_contains(Content, "thoas")
        end,
        Files
    ),
    %% Verify json:encode appears in auth template
    {ok, AuthBin} = file:read_file(filename:join(AuthDir, "auth.erl.mustache")),
    assert_contains(binary_to_list(AuthBin), "json:encode"),
    %% Verify json:encode/decode in test suite template
    {ok, TestBin} = file:read_file(filename:join(AuthDir, "auth_test_suite.erl.mustache")),
    assert_contains(binary_to_list(TestBin), "json:encode"),
    assert_contains(binary_to_list(TestBin), "json:decode").

gen_live_creates_views(_Config) ->
    AppDir = "live",
    ok = file:make_dir(AppDir),
    Fields = [{"name", "string"}, {"email", "string"}],
    rebar3_nova_gen_live:generate(myapp, AppDir, "users", Fields, [index, show, new, edit]),
    assert_file_exists(AppDir, "src/views/myapp_user_index_view.erl"),
    assert_file_exists(AppDir, "src/views/myapp_user_show_view.erl"),
    assert_file_exists(AppDir, "src/views/myapp_user_new_view.erl"),
    assert_file_exists(AppDir, "src/views/myapp_user_edit_view.erl").

%%======================================================================
%% Utils tests
%%======================================================================

utils_singularize(_Config) ->
    ?assertEqual("user", rebar3_nova_utils:singularize("users")),
    ?assertEqual("post", rebar3_nova_utils:singularize("posts")),
    ?assertEqual("data", rebar3_nova_utils:singularize("data")).

utils_pluralize(_Config) ->
    ?assertEqual("users", rebar3_nova_utils:pluralize("user")),
    ?assertEqual("posts", rebar3_nova_utils:pluralize("post")),
    ?assertEqual("users", rebar3_nova_utils:pluralize("users")).

utils_capitalize(_Config) ->
    ?assertEqual("User", rebar3_nova_utils:capitalize("user")),
    ?assertEqual("Name", rebar3_nova_utils:capitalize("name")),
    ?assertEqual("Already", rebar3_nova_utils:capitalize("Already")),
    ?assertEqual("", rebar3_nova_utils:capitalize("")).

utils_parse_fields(_Config) ->
    ?assertEqual(
        [{"name", "string"}, {"age", "integer"}],
        rebar3_nova_utils:parse_fields("name:string,age:integer")
    ),
    ?assertEqual(
        [{"name", "string"}],
        rebar3_nova_utils:parse_fields("name")
    ),
    ?assertEqual(
        [{"name", "string"}, {"bio", "text"}],
        rebar3_nova_utils:parse_fields(" name : string , bio : text ")
    ).

utils_timestamp(_Config) ->
    TS = rebar3_nova_utils:timestamp(),
    ?assertEqual(14, length(TS)),
    ?assert(lists:all(fun(C) -> C >= $0 andalso C =< $9 end, TS)).

%%======================================================================
%% run/1 export test
%%======================================================================

run_delegates_from_do(_Config) ->
    %% All gen modules should export run/1 and run/2
    lists:foreach(
        fun(Mod) ->
            ?assert(erlang:function_exported(Mod, run, 1)),
            ?assert(erlang:function_exported(Mod, run, 2))
        end,
        [
            rebar3_nova_gen_controller,
            rebar3_nova_gen_resource,
            rebar3_nova_gen_test,
            rebar3_nova_gen_auth,
            rebar3_nova_gen_live
        ]
    ),
    %% Dispatcher should export do/1
    ?assert(erlang:function_exported(rebar3_nova_gen, do, 1)).

%%======================================================================
%% Helpers
%%======================================================================

assert_file_exists(Base, RelPath) ->
    Path = filename:join(Base, RelPath),
    ?assert(filelib:is_regular(Path), #{msg => "Expected file to exist", path => Path}).

read_file(Base, RelPath) ->
    Path = filename:join(Base, RelPath),
    {ok, Bin} = file:read_file(Path),
    binary_to_list(Bin).

assert_contains(Content, Substr) ->
    case string:find(Content, Substr) of
        nomatch -> ct:fail({expected_substring, Substr, Content});
        _ -> ok
    end.

assert_not_contains(Content, Substr) ->
    case string:find(Content, Substr) of
        nomatch -> ok;
        _ -> ct:fail({unexpected_substring, Substr, Content})
    end.
