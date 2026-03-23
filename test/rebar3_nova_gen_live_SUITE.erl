-module(rebar3_nova_gen_live_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    generates_all_views/1,
    generates_schema/1,
    generates_migration/1,
    generates_test_suite/1,
    index_only/1,
    no_schema_flag/1,
    field_type_mapping/1,
    skips_existing_files/1,
    views_call_kura_directly/1,
    uses_correct_state_api/1
]).

all() ->
    [
        generates_all_views,
        generates_schema,
        generates_migration,
        generates_test_suite,
        index_only,
        no_schema_flag,
        field_type_mapping,
        skips_existing_files,
        views_call_kura_directly,
        uses_correct_state_api
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
%% Tests
%%======================================================================

generates_all_views(_Config) ->
    AppName = myapp,
    AppDir = "myapp",
    ok = file:make_dir(AppDir),
    Fields = [{"name", "string"}, {"email", "string"}, {"active", "boolean"}],
    Actions = [index, show, new, edit],

    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, Actions),

    assert_file_exists(AppDir, "src/views/myapp_user_index_view.erl"),
    assert_file_exists(AppDir, "src/views/myapp_user_show_view.erl"),
    assert_file_exists(AppDir, "src/views/myapp_user_new_view.erl"),
    assert_file_exists(AppDir, "src/views/myapp_user_edit_view.erl"),

    IndexContent = read_file(AppDir, "src/views/myapp_user_index_view.erl"),
    assert_contains(IndexContent, "-module(myapp_user_index_view)"),
    assert_contains(IndexContent, "-behaviour(arizona_view)"),
    assert_contains(IndexContent, "arizona_parse_transform"),
    assert_contains(IndexContent, "kura_repo_worker:all(myapp_repo"),
    assert_contains(IndexContent, "<th>Name</th>"),
    assert_contains(IndexContent, "<th>Email</th>"),
    assert_contains(IndexContent, "<th>Active</th>"),
    assert_contains(IndexContent, "handle_event"),
    assert_contains(IndexContent, "render_list"),

    ShowContent = read_file(AppDir, "src/views/myapp_user_show_view.erl"),
    assert_contains(ShowContent, "-module(myapp_user_show_view)"),
    assert_contains(ShowContent, "kura_repo_worker:get(myapp_repo, myapp_user, Id)"),
    assert_contains(ShowContent, "<dt>Name</dt>"),

    NewContent = read_file(AppDir, "src/views/myapp_user_new_view.erl"),
    assert_contains(NewContent, "-module(myapp_user_new_view)"),
    assert_contains(NewContent, "kura_changeset:cast(myapp_user"),
    assert_contains(NewContent, "kura_repo_worker:insert(myapp_repo"),
    assert_contains(NewContent, "<form"),

    EditContent = read_file(AppDir, "src/views/myapp_user_edit_view.erl"),
    assert_contains(EditContent, "-module(myapp_user_edit_view)"),
    assert_contains(EditContent, "kura_repo_worker:update(myapp_repo"),
    assert_contains(EditContent, "value=\"{maps:get(name, User)}\""),

    ok.

generates_schema(_Config) ->
    AppName = myapp,
    AppDir = "myapp_schema",
    ok = file:make_dir(AppDir),
    Fields = [{"name", "string"}, {"age", "integer"}],

    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, [index]),
    generate_optional(AppName, AppDir, "users", Fields, #{}),

    assert_file_exists(AppDir, "src/schemas/myapp_user.erl"),

    Content = read_file(AppDir, "src/schemas/myapp_user.erl"),
    assert_contains(Content, "-module(myapp_user)"),
    assert_contains(Content, "-behaviour(kura_schema)"),
    assert_contains(Content, "kura/include/kura.hrl"),
    assert_contains(Content, "<<\"users\">>"),
    assert_contains(Content, "name = id, type = id, primary_key = true"),
    assert_contains(Content, "name = name, type = string"),
    assert_contains(Content, "name = age, type = integer"),
    assert_contains(Content, "name = inserted_at, type = utc_datetime"),
    assert_contains(Content, "name = updated_at, type = utc_datetime"),

    ok.

generates_migration(_Config) ->
    AppName = myapp,
    AppDir = "myapp_mig",
    ok = file:make_dir(AppDir),
    Fields = [{"title", "string"}, {"body", "text"}],

    rebar3_nova_gen_live:generate(AppName, AppDir, "posts", Fields, [index]),
    generate_optional(AppName, AppDir, "posts", Fields, #{}),

    %% Find the migration file (has timestamp in name)
    MigDir = filename:join([AppDir, "src", "migrations"]),
    {ok, Files} = file:list_dir(MigDir),
    [MigFile] = [F || F <- Files, string:find(F, "create_posts") =/= nomatch],

    Content = read_file(AppDir, filename:join(["src", "migrations", MigFile])),
    assert_contains(Content, "-behaviour(kura_migration)"),
    assert_contains(Content, "create_table"),
    assert_contains(Content, "<<\"posts\">>"),
    assert_contains(Content, "name = title, type = string"),
    assert_contains(Content, "name = body, type = text"),
    assert_contains(Content, "drop_table"),

    ok.

generates_test_suite(_Config) ->
    AppName = myapp,
    AppDir = "myapp_test",
    ok = file:make_dir(AppDir),
    Fields = [{"name", "string"}],

    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, [index]),
    generate_optional(AppName, AppDir, "users", Fields, #{}),

    assert_file_exists(AppDir, "test/myapp_user_live_SUITE.erl"),

    Content = read_file(AppDir, "test/myapp_user_live_SUITE.erl"),
    assert_contains(Content, "-module(myapp_user_live_SUITE)"),
    assert_contains(Content, "common_test"),
    assert_contains(Content, "kura_repo_worker:all(myapp_repo"),

    ok.

index_only(_Config) ->
    AppName = myapp,
    AppDir = "myapp_idx",
    ok = file:make_dir(AppDir),
    Fields = [{"name", "string"}],

    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, [index]),

    assert_file_exists(AppDir, "src/views/myapp_user_index_view.erl"),
    assert_file_not_exists(AppDir, "src/views/myapp_user_show_view.erl"),
    assert_file_not_exists(AppDir, "src/views/myapp_user_new_view.erl"),
    assert_file_not_exists(AppDir, "src/views/myapp_user_edit_view.erl"),

    ok.

no_schema_flag(_Config) ->
    AppName = myapp,
    AppDir = "myapp_nos",
    ok = file:make_dir(AppDir),
    Fields = [{"name", "string"}],

    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, [index]),
    generate_optional(AppName, AppDir, "users", Fields, #{no_schema => true}),

    assert_file_exists(AppDir, "src/views/myapp_user_index_view.erl"),
    assert_file_not_exists(AppDir, "src/schemas/myapp_user.erl"),

    %% Migration should also be skipped
    MigDir = filename:join([AppDir, "src", "migrations"]),
    case filelib:is_dir(MigDir) of
        false ->
            ok;
        true ->
            {ok, Files} = file:list_dir(MigDir),
            ?assertEqual([], Files)
    end,

    ok.

field_type_mapping(_Config) ->
    AppName = myapp,
    AppDir = "myapp_types",
    ok = file:make_dir(AppDir),
    Fields = [
        {"name", "string"},
        {"bio", "text"},
        {"age", "integer"},
        {"score", "float"},
        {"active", "boolean"},
        {"birthday", "date"},
        {"created", "utc_datetime"}
    ],

    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, [new]),

    Content = read_file(AppDir, "src/views/myapp_user_new_view.erl"),
    assert_contains(Content, "type=\"text\" name=\"name\""),
    assert_contains(Content, "<textarea name=\"bio\""),
    assert_contains(Content, "type=\"number\" name=\"age\""),
    assert_contains(Content, "type=\"number\" name=\"score\""),
    assert_contains(Content, "type=\"checkbox\" name=\"active\""),
    assert_contains(Content, "type=\"date\" name=\"birthday\""),
    assert_contains(Content, "type=\"datetime-local\" name=\"created\""),

    ok.

skips_existing_files(_Config) ->
    AppName = myapp,
    AppDir = "myapp_skip",
    ok = file:make_dir(AppDir),
    Fields = [{"name", "string"}],

    %% Generate once
    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, [index]),
    Content1 = read_file(AppDir, "src/views/myapp_user_index_view.erl"),

    %% Generate again — should skip, content unchanged
    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, [index]),
    Content2 = read_file(AppDir, "src/views/myapp_user_index_view.erl"),

    ?assertEqual(Content1, Content2),

    ok.

views_call_kura_directly(_Config) ->
    AppName = myapp,
    AppDir = "myapp_direct",
    ok = file:make_dir(AppDir),
    Fields = [{"name", "string"}, {"email", "string"}],
    Actions = [index, show, new, edit],

    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, Actions),

    %% No context module generated
    assert_file_not_exists(AppDir, "src/contexts/myapp_users.erl"),

    %% Index calls kura directly
    IndexContent = read_file(AppDir, "src/views/myapp_user_index_view.erl"),
    assert_contains(IndexContent, "kura_repo_worker:all(myapp_repo"),
    assert_contains(IndexContent, "kura_query:from(myapp_user)"),
    assert_contains(IndexContent, "kura_repo_worker:delete(myapp_repo"),

    %% New calls kura directly
    NewContent = read_file(AppDir, "src/views/myapp_user_new_view.erl"),
    assert_contains(NewContent, "kura_changeset:cast(myapp_user, #{}, Params, [name, email])"),
    assert_contains(NewContent, "kura_changeset:validate_required"),
    assert_contains(NewContent, "kura_repo_worker:insert(myapp_repo"),

    %% Edit calls kura directly
    EditContent = read_file(AppDir, "src/views/myapp_user_edit_view.erl"),
    assert_contains(
        EditContent, "kura_changeset:cast(myapp_user, Existing, Params, [name, email])"
    ),
    assert_contains(EditContent, "kura_repo_worker:update(myapp_repo"),

    ok.

uses_correct_state_api(_Config) ->
    AppName = myapp,
    AppDir = "myapp_state",
    ok = file:make_dir(AppDir),
    Fields = [{"name", "string"}],
    Actions = [index, new, edit],

    rebar3_nova_gen_live:generate(AppName, AppDir, "users", Fields, Actions),

    %% Index uses get_state/update_state for handle_event
    IndexContent = read_file(AppDir, "src/views/myapp_user_index_view.erl"),
    assert_contains(IndexContent, "arizona_view:get_state(View)"),
    assert_contains(IndexContent, "arizona_stateful:put_binding("),
    assert_contains(IndexContent, "arizona_view:update_state("),

    %% New uses get_state/update_state for error handling
    NewContent = read_file(AppDir, "src/views/myapp_user_new_view.erl"),
    assert_contains(NewContent, "arizona_view:get_state(View)"),
    assert_contains(NewContent, "arizona_stateful:put_binding(errors"),

    %% Edit uses get_state for reading bindings
    EditContent = read_file(AppDir, "src/views/myapp_user_edit_view.erl"),
    assert_contains(EditContent, "arizona_view:get_state(View)"),
    assert_contains(EditContent, "arizona_stateful:get_binding(user, State0)"),

    ok.

%%======================================================================
%% Helpers
%%======================================================================

generate_optional(AppName, AppDir, Name, Fields, Opts) ->
    rebar3_nova_gen_live:generate_optional(AppName, AppDir, Name, Fields, Opts).

assert_file_exists(Base, RelPath) ->
    Path = filename:join(Base, RelPath),
    ?assert(filelib:is_regular(Path), #{msg => "Expected file to exist", path => Path}).

assert_file_not_exists(Base, RelPath) ->
    Path = filename:join(Base, RelPath),
    ?assertNot(filelib:is_regular(Path), #{msg => "Expected file to not exist", path => Path}).

read_file(Base, RelPath) ->
    Path = filename:join(Base, RelPath),
    {ok, Bin} = file:read_file(Path),
    binary_to_list(Bin).

assert_contains(Content, Substr) ->
    case string:find(Content, Substr) of
        nomatch ->
            ct:fail({expected_substring, Substr, Content});
        _ ->
            ok
    end.
