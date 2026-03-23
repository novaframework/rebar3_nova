-module(rebar3_nova_gen_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    gen_controller_in_project/1,
    gen_resource_in_project/1,
    gen_test_in_project/1,
    gen_auth_in_project/1,
    gen_live_in_project/1,
    all_generated_files_parse/1,
    project_compiles/1
]).

all() ->
    [{group, integration}].

groups() ->
    [
        {integration, [sequence], [
            gen_controller_in_project,
            gen_resource_in_project,
            gen_test_in_project,
            gen_auth_in_project,
            gen_live_in_project,
            all_generated_files_parse,
            project_compiles
        ]}
    ].

init_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    ProjectDir = filename:join(PrivDir, "testapp"),
    Flags = #{
        kura => true,
        pgo => false,
        arizona => false,
        lfe => false,
        ci => false,
        docker => false,
        otel => false
    },
    {ok, OldCwd} = file:get_cwd(),
    ok = file:set_cwd(PrivDir),
    rebar3_nova_new:generate_project("testapp", Flags),
    ok = file:set_cwd(OldCwd),
    [{project_dir, ProjectDir}, {app_name, testapp} | Config].

end_per_suite(_Config) ->
    ok.

%%======================================================================
%% Generator tests — run each generator against the scaffolded project
%%======================================================================

gen_controller_in_project(Config) ->
    ProjectDir = ?config(project_dir, Config),
    AppName = ?config(app_name, Config),
    rebar3_nova_gen_controller:generate(AppName, ProjectDir, "products", [list, show, create]),
    assert_file_exists(ProjectDir, "src/controllers/testapp_products_controller.erl"),
    Content = read_file(ProjectDir, "src/controllers/testapp_products_controller.erl"),
    assert_contains(Content, "-module(testapp_products_controller)"),
    assert_contains(Content, "list/1, show/1, create/1"),
    assert_contains(Content, "list(#{req := _Req}"),
    assert_contains(Content, "show(#{req := _Req}"),
    assert_contains(Content, "{status, 201"),
    ok.

gen_resource_in_project(Config) ->
    ProjectDir = ?config(project_dir, Config),
    AppName = ?config(app_name, Config),
    rebar3_nova_gen_controller:generate(AppName, ProjectDir, "orders", [
        list, show, create, update, delete
    ]),
    CtrlFile = "src/controllers/testapp_orders_controller.erl",
    assert_file_exists(ProjectDir, CtrlFile),
    Content = read_file(ProjectDir, CtrlFile),
    assert_contains(Content, "-module(testapp_orders_controller)"),
    assert_contains(Content, "list/1, show/1, create/1, update/1, delete/1"),
    ok.

gen_test_in_project(Config) ->
    ProjectDir = ?config(project_dir, Config),
    AppName = ?config(app_name, Config),
    SuiteName = lists:flatten(io_lib:format("~s_products_controller_SUITE", [AppName])),
    FileName = filename:join([ProjectDir, "test", SuiteName ++ ".erl"]),
    Context = #{suite => SuiteName, app => atom_to_list(AppName), name => "products"},
    Content = rebar3_nova_utils:render_template(["gen", "test_suite.erl.mustache"], Context),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content),
    assert_file_exists(ProjectDir, "test/testapp_products_controller_SUITE.erl"),
    FileContent = read_file(ProjectDir, "test/testapp_products_controller_SUITE.erl"),
    assert_contains(FileContent, "-module(testapp_products_controller_SUITE)"),
    assert_contains(FileContent, "test_list"),
    assert_contains(FileContent, "test_create"),
    assert_contains(FileContent, "httpc:request"),
    assert_contains(FileContent, "localhost:8080/products"),
    ok.

gen_auth_in_project(Config) ->
    ProjectDir = ?config(project_dir, Config),
    App = "testapp",
    Ctx = #{
        app => App,
        user_mod => App ++ "_user",
        token_mod => App ++ "_user_token",
        accounts => App ++ "_accounts",
        repo => App ++ "_repo"
    },
    generate_auth_files(ProjectDir, Ctx),

    %% Verify all 9 files exist
    MigDir = filename:join([ProjectDir, "src", "migrations"]),
    {ok, MigFiles} = file:list_dir(MigDir),
    AuthMigs = [F || F <- MigFiles, string:find(F, "create_auth_tables") =/= nomatch],
    ?assertEqual(1, length(AuthMigs)),

    assert_file_exists(ProjectDir, "src/schemas/testapp_user.erl"),
    assert_file_exists(ProjectDir, "src/schemas/testapp_user_token.erl"),
    assert_file_exists(ProjectDir, "src/testapp_accounts.erl"),
    assert_file_exists(ProjectDir, "src/testapp_auth.erl"),
    assert_file_exists(ProjectDir, "src/controllers/testapp_session_controller.erl"),
    assert_file_exists(ProjectDir, "src/controllers/testapp_registration_controller.erl"),
    assert_file_exists(ProjectDir, "src/controllers/testapp_user_controller.erl"),
    assert_file_exists(ProjectDir, "test/testapp_auth_SUITE.erl"),

    %% Verify auth module uses json, not thoas
    AuthContent = read_file(ProjectDir, "src/testapp_auth.erl"),
    assert_contains(AuthContent, "-module(testapp_auth)"),
    assert_contains(AuthContent, "require_authenticated"),
    assert_contains(AuthContent, "json:encode"),
    assert_not_contains(AuthContent, "thoas"),

    %% Verify accounts module
    AccContent = read_file(ProjectDir, "src/testapp_accounts.erl"),
    assert_contains(AccContent, "-module(testapp_accounts)"),
    assert_contains(AccContent, "register_user"),
    assert_contains(AccContent, "testapp_user:registration_changeset"),
    assert_contains(AccContent, "testapp_repo:insert"),
    assert_contains(AccContent, "crypto:hash_equals"),

    %% Verify user schema
    UserContent = read_file(ProjectDir, "src/schemas/testapp_user.erl"),
    assert_contains(UserContent, "-behaviour(kura_schema)"),
    assert_contains(UserContent, "registration_changeset"),
    assert_contains(UserContent, "bcrypt:hashpw"),

    %% Verify test suite uses json, not thoas
    TestContent = read_file(ProjectDir, "test/testapp_auth_SUITE.erl"),
    assert_contains(TestContent, "json:encode"),
    assert_contains(TestContent, "json:decode"),
    assert_not_contains(TestContent, "thoas"),

    %% Verify session controller
    SessionContent = read_file(ProjectDir, "src/controllers/testapp_session_controller.erl"),
    assert_contains(SessionContent, "testapp_accounts:get_user_by_email_and_password"),
    assert_contains(SessionContent, "nova_session:set"),

    ok.

gen_live_in_project(Config) ->
    ProjectDir = ?config(project_dir, Config),
    AppName = ?config(app_name, Config),
    Fields = [{"title", "string"}, {"body", "text"}, {"published", "boolean"}],
    Actions = [index, show, new, edit],
    rebar3_nova_gen_live:generate(AppName, ProjectDir, "articles", Fields, Actions),
    rebar3_nova_gen_live:generate_optional(AppName, ProjectDir, "articles", Fields, #{}),

    %% Views
    assert_file_exists(ProjectDir, "src/views/testapp_article_index_view.erl"),
    assert_file_exists(ProjectDir, "src/views/testapp_article_show_view.erl"),
    assert_file_exists(ProjectDir, "src/views/testapp_article_new_view.erl"),
    assert_file_exists(ProjectDir, "src/views/testapp_article_edit_view.erl"),

    %% Schema + migration
    assert_file_exists(ProjectDir, "src/schemas/testapp_article.erl"),
    MigDir = filename:join([ProjectDir, "src", "migrations"]),
    {ok, MigFiles} = file:list_dir(MigDir),
    ArticleMigs = [F || F <- MigFiles, string:find(F, "create_articles") =/= nomatch],
    ?assertEqual(1, length(ArticleMigs)),

    %% Test suite
    assert_file_exists(ProjectDir, "test/testapp_article_live_SUITE.erl"),

    %% Content checks
    IndexContent = read_file(ProjectDir, "src/views/testapp_article_index_view.erl"),
    assert_contains(IndexContent, "-module(testapp_article_index_view)"),
    assert_contains(IndexContent, "arizona_parse_transform"),
    assert_contains(IndexContent, "kura_repo_worker:all(testapp_repo"),
    assert_contains(IndexContent, "<th>Title</th>"),
    assert_contains(IndexContent, "<th>Body</th>"),
    assert_contains(IndexContent, "<th>Published</th>"),

    NewContent = read_file(ProjectDir, "src/views/testapp_article_new_view.erl"),
    assert_contains(NewContent, "type=\"text\" name=\"title\""),
    assert_contains(NewContent, "<textarea name=\"body\""),
    assert_contains(NewContent, "type=\"checkbox\" name=\"published\""),
    assert_contains(
        NewContent, "kura_changeset:cast(testapp_article, #{}, Params, [title, body, published])"
    ),

    SchemaContent = read_file(ProjectDir, "src/schemas/testapp_article.erl"),
    assert_contains(SchemaContent, "-behaviour(kura_schema)"),
    assert_contains(SchemaContent, "name = title, type = string"),
    assert_contains(SchemaContent, "name = body, type = text"),
    assert_contains(SchemaContent, "name = published, type = boolean"),

    ok.

%%======================================================================
%% Syntax validation — parse all generated .erl files
%%======================================================================

all_generated_files_parse(Config) ->
    ProjectDir = ?config(project_dir, Config),
    ErlFiles = find_erl_files(ProjectDir),
    ct:pal("Found ~p .erl files to validate", [length(ErlFiles)]),
    ?assert(length(ErlFiles) >= 15, #{msg => "Expected at least 15 .erl files"}),
    lists:foreach(
        fun(File) ->
            validate_erl_syntax(File)
        end,
        ErlFiles
    ),
    ok.

%%======================================================================
%% Full compilation test — runs rebar3 compile on the generated project
%%======================================================================

project_compiles(Config) ->
    case os:getenv("SKIP_COMPILE_TEST") of
        false ->
            do_compile_test(Config);
        _ ->
            ct:pal("Skipping compile test (set SKIP_COMPILE_TEST=false to enable)"),
            {skip, "SKIP_COMPILE_TEST is set"}
    end.

do_compile_test(Config) ->
    ProjectDir = ?config(project_dir, Config),

    %% Patch rebar.config: add bcrypt dep for gen_auth
    RebarConfig = filename:join(ProjectDir, "rebar.config"),
    {ok, OrigBin} = file:read_file(RebarConfig),
    OrigContent = binary_to_list(OrigBin),

    Patched = string:replace(OrigContent, "    kura\n", "    kura,\n    bcrypt\n", leading),
    ok = file:write_file(RebarConfig, Patched),

    ct:pal("Running rebar3 compile in ~s", [ProjectDir]),
    {ok, OldCwd} = file:get_cwd(),
    ok = file:set_cwd(ProjectDir),
    %% NO_COLOR avoids ANSI escape codes that break ct:pal
    Result = os:cmd("NO_COLOR=1 rebar3 compile 2>&1"),
    ok = file:set_cwd(OldCwd),
    ct:pal("Compile output (~p bytes)", [length(Result)]),

    %% Check for success: should contain "Compiling" and not "Error"
    HasCompiling = string:find(Result, "Compiling") =/= nomatch,
    HasError = string:find(Result, "Error") =/= nomatch,
    case {HasCompiling, HasError} of
        {true, false} -> ok;
        {_, true} -> ct:fail({compile_errors, Result});
        {false, _} -> ct:fail({compile_failed, Result})
    end,
    ok.

%%======================================================================
%% Helpers
%%======================================================================

generate_auth_files(ProjectDir, #{app := App} = Ctx) ->
    TS = rebar3_nova_utils:timestamp(),
    Mod = "m" ++ TS ++ "_create_auth_tables",
    MigFile = filename:join([ProjectDir, "src", "migrations", Mod ++ ".erl"]),
    MigContent = rebar3_nova_utils:render_template(
        ["gen", "auth", "migration.erl.mustache"], Ctx#{mod => Mod}
    ),
    rebar3_nova_utils:write_file_if_not_exists(MigFile, MigContent),

    #{user_mod := UserMod, token_mod := TokenMod, accounts := Accounts} = Ctx,

    write_template(
        ProjectDir,
        ["src", "schemas", UserMod ++ ".erl"],
        ["gen", "auth", "user_schema.erl.mustache"],
        Ctx#{mod => UserMod}
    ),
    write_template(
        ProjectDir,
        ["src", "schemas", TokenMod ++ ".erl"],
        ["gen", "auth", "user_token_schema.erl.mustache"],
        Ctx#{mod => TokenMod}
    ),
    write_template(
        ProjectDir,
        ["src", Accounts ++ ".erl"],
        ["gen", "auth", "accounts.erl.mustache"],
        Ctx#{mod => Accounts}
    ),

    AuthMod = App ++ "_auth",
    write_template(
        ProjectDir,
        ["src", AuthMod ++ ".erl"],
        ["gen", "auth", "auth.erl.mustache"],
        Ctx#{mod => AuthMod}
    ),

    SessionMod = App ++ "_session_controller",
    write_template(
        ProjectDir,
        ["src", "controllers", SessionMod ++ ".erl"],
        ["gen", "auth", "session_controller.erl.mustache"],
        Ctx#{mod => SessionMod}
    ),

    RegMod = App ++ "_registration_controller",
    write_template(
        ProjectDir,
        ["src", "controllers", RegMod ++ ".erl"],
        ["gen", "auth", "registration_controller.erl.mustache"],
        Ctx#{mod => RegMod}
    ),

    UserCtrlMod = App ++ "_user_controller",
    write_template(
        ProjectDir,
        ["src", "controllers", UserCtrlMod ++ ".erl"],
        ["gen", "auth", "user_controller.erl.mustache"],
        Ctx#{mod => UserCtrlMod}
    ),

    Suite = App ++ "_auth_SUITE",
    write_template(
        ProjectDir,
        ["test", Suite ++ ".erl"],
        ["gen", "auth", "auth_test_suite.erl.mustache"],
        Ctx#{suite => Suite}
    ).

write_template(ProjectDir, RelPath, TemplatePath, Ctx) ->
    FileName = filename:join([ProjectDir | RelPath]),
    Content = rebar3_nova_utils:render_template(TemplatePath, Ctx),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

find_erl_files(Dir) ->
    find_erl_files(Dir, []).

find_erl_files(Dir, Acc) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foldl(
                fun(F, A) ->
                    Path = filename:join(Dir, F),
                    case filelib:is_dir(Path) of
                        true ->
                            find_erl_files(Path, A);
                        false ->
                            case filename:extension(F) of
                                ".erl" -> [Path | A];
                                _ -> A
                            end
                    end
                end,
                Acc,
                Files
            );
        {error, _} ->
            Acc
    end.

validate_erl_syntax(File) ->
    {ok, Bin} = file:read_file(File),
    Content = binary_to_list(Bin),
    %% Skip files that need preprocessor/parse transforms
    Skip =
        string:find(Content, "arizona_parse_transform") =/= nomatch orelse
            string:find(Content, "-define(") =/= nomatch orelse
            string:find(Content, "-include") =/= nomatch,
    case Skip of
        true ->
            ct:pal("Skipping full parse for ~s (macros/includes/transforms)", [
                filename:basename(File)
            ]),
            %% Still validate tokenization
            validate_tokenization(File, Content);
        false ->
            do_validate_syntax(File, Content)
    end.

validate_tokenization(File, Content) ->
    case erl_scan:string(Content, 1, [text]) of
        {ok, _Tokens, _EndLoc} ->
            ok;
        {error, {_Loc, Mod, Desc}, _} ->
            ct:fail({scan_error, File, Mod:format_error(Desc)})
    end.

do_validate_syntax(File, Content) ->
    case erl_scan:string(Content, 1, [text]) of
        {ok, Tokens, _EndLoc} ->
            validate_forms(File, Tokens);
        {error, {_Loc, Mod, Desc}, _} ->
            ct:fail({scan_error, File, Mod:format_error(Desc)})
    end.

validate_forms(File, Tokens) ->
    case split_forms(Tokens) of
        {ok, Forms} ->
            lists:foreach(
                fun(FormTokens) ->
                    case erl_parse:parse_form(FormTokens) of
                        {ok, _} ->
                            ok;
                        {error, {_Loc, Mod, Desc}} ->
                            ct:fail({parse_error, File, Mod:format_error(Desc), FormTokens})
                    end
                end,
                Forms
            );
        {error, Reason} ->
            ct:fail({form_split_error, File, Reason})
    end.

split_forms(Tokens) ->
    split_forms(Tokens, [], []).

split_forms([], [], Acc) ->
    {ok, lists:reverse(Acc)};
split_forms([], Current, Acc) ->
    {ok, lists:reverse([lists:reverse(Current) | Acc])};
split_forms([{dot, _} = Dot | Rest], Current, Acc) ->
    Form = lists:reverse([Dot | Current]),
    split_forms(Rest, [], [Form | Acc]);
split_forms([Token | Rest], Current, Acc) ->
    split_forms(Rest, [Token | Current], Acc).

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
