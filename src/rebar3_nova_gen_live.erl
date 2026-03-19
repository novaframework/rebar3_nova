-module(rebar3_nova_gen_live).

-export([init/1, do/1, format_error/1]).
-export([generate/5, generate_optional/5]).

-define(PROVIDER, gen_live).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example,
            "rebar3 nova gen_live --name users --fields name:string,email:string,active:boolean"},
        {opts, [
            {name, $n, "name", string, "Resource name, plural (required)"},
            {fields, $f, "fields", string, "Comma-separated field:type pairs (required)"},
            {actions, $a, "actions", {string, "index,show,new,edit"},
                "Comma-separated view actions"},
            {no_schema, undefined, "no-schema", boolean, "Skip schema and migration generation"}
        ]},
        {short_desc, "Generate Arizona LiveView CRUD views for a resource"},
        {desc,
            "Generates Arizona views, Kura schema, migration, and test suite\n"
            "for a resource. Similar to Phoenix's phx.gen.live.\n\n"
            "Example:\n"
            "  rebar3 nova gen_live --name users --fields name:string,email:string,active:boolean\n\n"
            "Supported field types: string, text, integer, float, boolean, date,\n"
            "  utc_datetime, uuid, jsonb"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case {proplists:get_value(name, Args), proplists:get_value(fields, Args)} of
        {undefined, _} ->
            rebar_api:abort("--name is required", []);
        {_, undefined} ->
            rebar_api:abort("--fields is required", []);
        {Name, FieldsStr} ->
            AppName = rebar3_nova_utils:get_app_name(State),
            AppDir = rebar3_nova_utils:get_app_dir(State),
            ActionsStr = proplists:get_value(actions, Args, "index,show,new,edit"),
            Actions = rebar3_nova_utils:parse_actions(ActionsStr),
            Fields = parse_fields(FieldsStr),
            Opts = #{
                no_schema => proplists:get_value(no_schema, Args, false)
            },
            generate(AppName, AppDir, Name, Fields, Actions),
            generate_optional(AppName, AppDir, Name, Fields, Opts),
            print_route_hints(AppName, Name, Actions),
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec generate(atom(), file:filename(), string(), [{string(), string()}], [atom()]) -> ok.
generate(AppName, AppDir, Name, Fields, Actions) ->
    App = atom_to_list(AppName),
    Singular = singularize(Name),
    lists:foreach(
        fun(Action) ->
            generate_view(App, AppDir, Singular, Name, Fields, Action)
        end,
        Actions
    ).

%%======================================================================
%% Internal: optional generators (schema, migration, test)
%%======================================================================

generate_optional(AppName, AppDir, Name, Fields, Opts) ->
    App = atom_to_list(AppName),
    Singular = singularize(Name),
    case maps:get(no_schema, Opts, false) of
        false ->
            generate_schema(App, AppDir, Singular, Name, Fields),
            generate_migration(AppDir, Name, Fields);
        true ->
            ok
    end,
    generate_test(App, AppDir, Singular, Name, Fields).

%%======================================================================
%% Internal: view generators
%%======================================================================

generate_view(App, AppDir, Singular, Plural, Fields, index) ->
    Mod = App ++ "_" ++ Singular ++ "_index_view",
    FileName = filename:join([AppDir, "src", "views", Mod ++ ".erl"]),
    Content = index_view_content(Mod, App, Singular, Plural, Fields),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content);
generate_view(App, AppDir, Singular, _Plural, Fields, show) ->
    Mod = App ++ "_" ++ Singular ++ "_show_view",
    FileName = filename:join([AppDir, "src", "views", Mod ++ ".erl"]),
    Content = show_view_content(Mod, App, Singular, Fields),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content);
generate_view(App, AppDir, Singular, _Plural, Fields, new) ->
    Mod = App ++ "_" ++ Singular ++ "_new_view",
    FileName = filename:join([AppDir, "src", "views", Mod ++ ".erl"]),
    Content = new_view_content(Mod, App, Singular, Fields),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content);
generate_view(App, AppDir, Singular, _Plural, Fields, edit) ->
    Mod = App ++ "_" ++ Singular ++ "_edit_view",
    FileName = filename:join([AppDir, "src", "views", Mod ++ ".erl"]),
    Content = edit_view_content(Mod, App, Singular, Fields),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content);
generate_view(_App, _AppDir, _Singular, _Plural, _Fields, _Action) ->
    ok.

%%----------------------------------------------------------------------
%% Index view
%%----------------------------------------------------------------------

index_view_content(Mod, App, Singular, Plural, Fields) ->
    Schema = App ++ "_" ++ Singular,
    Repo = App ++ "_repo",
    iolist_to_binary([
        "-module(",
        Mod,
        ").\n"
        "-compile({parse_transform, arizona_parse_transform}).\n"
        "-behaviour(arizona_view).\n\n"
        "-export([mount/2, render/1, handle_event/3]).\n\n"
        "mount(_MountArg, _Request) ->\n"
        "    {ok, Items} = kura_repo_worker:all(",
        Repo,
        ", kura_query:from(",
        Schema,
        ")),\n"
        "    arizona_view:new(?MODULE, #{",
        Plural,
        " => Items}, none).\n\n"
        "render(Bindings) ->\n"
        "    arizona_template:from_html(~\"\"\"\"\n"
        "    <div class=\"container\">\n"
        "        <h1>",
        capitalize(Plural),
        "</h1>\n"
        "        <a href=\"/",
        Plural,
        "/new\" class=\"btn\">New ",
        capitalize(Singular),
        "</a>\n"
        "        <table>\n"
        "            <thead>\n"
        "                <tr>\n",
        table_headers(Fields),
        "                    <th></th>\n"
        "                </tr>\n"
        "            </thead>\n"
        "            <tbody>\n"
        "                {arizona_template:render_list(\n"
        "                    fun(Item) ->\n"
        "                        arizona_template:from_html(~\"\"\"\n"
        "                        <tr>\n",
        table_cells(Fields),
        "                            <td>\n"
        "                                <a href=\"/",
        Plural,
        "/{maps:get(id, Item)}\">Show</a>\n"
        "                                <a href=\"/",
        Plural,
        "/{maps:get(id, Item)}/edit\">Edit</a>\n"
        "                            </td>\n"
        "                        </tr>\n"
        "                        \"\"\")\n"
        "                    end, arizona_template:get_binding(",
        Plural,
        ", Bindings))}\n"
        "            </tbody>\n"
        "        </table>\n"
        "    </div>\n"
        "    \"\"\"\").\n\n"
        "handle_event(~\"delete\", #{~\"id\" := Id}, View) ->\n"
        "    {ok, Record} = kura_repo_worker:get(",
        Repo,
        ", ",
        Schema,
        ", Id),\n"
        "    CS = kura_changeset:cast(",
        Schema,
        ", Record, #{}, []),\n"
        "    {ok, _} = kura_repo_worker:delete(",
        Repo,
        ", CS),\n"
        "    {ok, Items} = kura_repo_worker:all(",
        Repo,
        ", kura_query:from(",
        Schema,
        ")),\n"
        "    State = arizona_view:get_state(View),\n"
        "    NewState = arizona_stateful:put_binding(",
        Plural,
        ", Items, State),\n"
        "    {[], arizona_view:update_state(NewState, View)}.\n"
    ]).

table_headers(Fields) ->
    iolist_to_binary([
        ["                    <th>", capitalize(Name), "</th>\n"]
     || {Name, _Type} <- Fields
    ]).

table_cells(Fields) ->
    iolist_to_binary([
        ["                            <td>{maps:get(", Name, ", Item)}</td>\n"]
     || {Name, _Type} <- Fields
    ]).

%%----------------------------------------------------------------------
%% Show view
%%----------------------------------------------------------------------

show_view_content(Mod, App, Singular, Fields) ->
    Schema = App ++ "_" ++ Singular,
    Repo = App ++ "_repo",
    iolist_to_binary([
        "-module(",
        Mod,
        ").\n"
        "-compile({parse_transform, arizona_parse_transform}).\n"
        "-behaviour(arizona_view).\n\n"
        "-export([mount/2, render/1]).\n\n"
        "mount(#{id := Id}, _Request) ->\n"
        "    {ok, Item} = kura_repo_worker:get(",
        Repo,
        ", ",
        Schema,
        ", Id),\n"
        "    arizona_view:new(?MODULE, #{",
        Singular,
        " => Item}, none).\n\n"
        "render(Bindings) ->\n"
        "    ",
        capitalize(Singular),
        " = arizona_template:get_binding(",
        Singular,
        ", Bindings),\n"
        "    arizona_template:from_html(~\"\"\"\n"
        "    <div class=\"container\">\n"
        "        <h1>",
        capitalize(Singular),
        "</h1>\n"
        "        <dl>\n",
        show_fields(Singular, Fields),
        "        </dl>\n"
        "        <a href=\"/",
        pluralize(Singular),
        "\">Back</a>\n"
        "        <a href=\"/",
        pluralize(Singular),
        "/{maps:get(id, ",
        capitalize(Singular),
        ")}/edit\">Edit</a>\n"
        "    </div>\n"
        "    \"\"\").\n"
    ]).

show_fields(_Singular, Fields) ->
    iolist_to_binary([
        [
            "            <dt>",
            capitalize(Name),
            "</dt>\n"
            "            <dd>{maps:get(",
            Name,
            ", ",
            capitalize(_Singular),
            ")}</dd>\n"
        ]
     || {Name, _Type} <- Fields
    ]).

%%----------------------------------------------------------------------
%% New view
%%----------------------------------------------------------------------

new_view_content(Mod, App, Singular, Fields) ->
    Schema = App ++ "_" ++ Singular,
    Repo = App ++ "_repo",
    CastFields = field_names_list(Fields),
    iolist_to_binary([
        "-module(",
        Mod,
        ").\n"
        "-compile({parse_transform, arizona_parse_transform}).\n"
        "-behaviour(arizona_view).\n\n"
        "-export([mount/2, render/1, handle_event/3]).\n\n"
        "mount(_MountArg, _Request) ->\n"
        "    arizona_view:new(?MODULE, #{errors => []}, none).\n\n"
        "render(Bindings) ->\n"
        "    arizona_template:from_html(~\"\"\"\n"
        "    <div class=\"container\">\n"
        "        <h1>New ",
        capitalize(Singular),
        "</h1>\n"
        "        <form onsubmit=\"arizona.pushEvent('save', this)\">\n",
        form_fields(Fields),
        "            <button type=\"submit\">Create</button>\n"
        "            <a href=\"/",
        pluralize(Singular),
        "\">Cancel</a>\n"
        "        </form>\n"
        "    </div>\n"
        "    \"\"\").\n\n"
        "handle_event(~\"save\", Params, View) ->\n"
        "    CS = kura_changeset:cast(",
        Schema,
        ", #{}, Params, ",
        CastFields,
        "),\n"
        "    CS1 = kura_changeset:validate_required(CS, ",
        CastFields,
        "),\n"
        "    case kura_repo_worker:insert(",
        Repo,
        ", CS1) of\n"
        "        {ok, _} ->\n"
        "            {[{redirect, ~\"/",
        pluralize(Singular),
        "\", #{}}], View};\n"
        "        {error, Changeset} ->\n"
        "            Errors = maps:get(errors, Changeset, []),\n"
        "            State = arizona_view:get_state(View),\n"
        "            NewState = arizona_stateful:put_binding(errors, Errors, State),\n"
        "            {[], arizona_view:update_state(NewState, View)}\n"
        "    end.\n"
    ]).

%%----------------------------------------------------------------------
%% Edit view
%%----------------------------------------------------------------------

edit_view_content(Mod, App, Singular, Fields) ->
    Schema = App ++ "_" ++ Singular,
    Repo = App ++ "_repo",
    CastFields = field_names_list(Fields),
    iolist_to_binary([
        "-module(",
        Mod,
        ").\n"
        "-compile({parse_transform, arizona_parse_transform}).\n"
        "-behaviour(arizona_view).\n\n"
        "-export([mount/2, render/1, handle_event/3]).\n\n"
        "mount(#{id := Id}, _Request) ->\n"
        "    {ok, Item} = kura_repo_worker:get(",
        Repo,
        ", ",
        Schema,
        ", Id),\n"
        "    arizona_view:new(?MODULE, #{",
        Singular,
        " => Item, errors => []}, none).\n\n"
        "render(Bindings) ->\n"
        "    ",
        capitalize(Singular),
        " = arizona_template:get_binding(",
        Singular,
        ", Bindings),\n"
        "    arizona_template:from_html(~\"\"\"\n"
        "    <div class=\"container\">\n"
        "        <h1>Edit ",
        capitalize(Singular),
        "</h1>\n"
        "        <form onsubmit=\"arizona.pushEvent('save', this)\">\n",
        edit_form_fields(Singular, Fields),
        "            <button type=\"submit\">Update</button>\n"
        "            <a href=\"/",
        pluralize(Singular),
        "\">Cancel</a>\n"
        "        </form>\n"
        "    </div>\n"
        "    \"\"\").\n\n"
        "handle_event(~\"save\", Params, View) ->\n"
        "    State0 = arizona_view:get_state(View),\n"
        "    ",
        capitalize(Singular),
        " = arizona_stateful:get_binding(",
        Singular,
        ", State0),\n"
        "    Id = maps:get(id, ",
        capitalize(Singular),
        "),\n"
        "    {ok, Existing} = kura_repo_worker:get(",
        Repo,
        ", ",
        Schema,
        ", Id),\n"
        "    CS = kura_changeset:cast(",
        Schema,
        ", Existing, Params, ",
        CastFields,
        "),\n"
        "    case kura_repo_worker:update(",
        Repo,
        ", CS) of\n"
        "        {ok, _} ->\n"
        "            {[{redirect, ~\"/",
        pluralize(Singular),
        "\", #{}}], View};\n"
        "        {error, Changeset} ->\n"
        "            Errors = maps:get(errors, Changeset, []),\n"
        "            NewState = arizona_stateful:put_binding(errors, Errors, State0),\n"
        "            {[], arizona_view:update_state(NewState, View)}\n"
        "    end.\n"
    ]).

%%======================================================================
%% Internal: form field generation
%%======================================================================

form_fields(Fields) ->
    iolist_to_binary([form_field(Name, Type) || {Name, Type} <- Fields]).

form_field(Name, Type) ->
    Label = capitalize(Name),
    InputType = field_to_input_type(Type),
    case InputType of
        "textarea" ->
            [
                "            <label>",
                Label,
                "</label>\n"
                "            <textarea name=\"",
                Name,
                "\"></textarea>\n"
            ];
        "checkbox" ->
            [
                "            <label>\n"
                "                <input type=\"checkbox\" name=\"",
                Name,
                "\" />\n"
                "                ",
                Label,
                "\n"
                "            </label>\n"
            ];
        _ ->
            [
                "            <label>",
                Label,
                "</label>\n"
                "            <input type=\"",
                InputType,
                "\" name=\"",
                Name,
                "\" />\n"
            ]
    end.

edit_form_fields(Singular, Fields) ->
    iolist_to_binary([edit_form_field(Singular, Name, Type) || {Name, Type} <- Fields]).

edit_form_field(Singular, Name, Type) ->
    Label = capitalize(Name),
    InputType = field_to_input_type(Type),
    Value = "maps:get(" ++ Name ++ ", " ++ capitalize(Singular) ++ ")",
    case InputType of
        "textarea" ->
            [
                "            <label>",
                Label,
                "</label>\n"
                "            <textarea name=\"",
                Name,
                "\">{",
                Value,
                "}</textarea>\n"
            ];
        "checkbox" ->
            [
                "            <label>\n"
                "                <input type=\"checkbox\" name=\"",
                Name,
                "\""
                " checked=\"{",
                Value,
                "}\" />\n"
                "                ",
                Label,
                "\n"
                "            </label>\n"
            ];
        _ ->
            [
                "            <label>",
                Label,
                "</label>\n"
                "            <input type=\"",
                InputType,
                "\" name=\"",
                Name,
                "\""
                " value=\"{",
                Value,
                "}\" />\n"
            ]
    end.

field_to_input_type("string") -> "text";
field_to_input_type("text") -> "textarea";
field_to_input_type("integer") -> "number";
field_to_input_type("float") -> "number";
field_to_input_type("boolean") -> "checkbox";
field_to_input_type("date") -> "date";
field_to_input_type("utc_datetime") -> "datetime-local";
field_to_input_type("uuid") -> "text";
field_to_input_type("jsonb") -> "textarea";
field_to_input_type(_) -> "text".

%%======================================================================
%% Internal: schema generator
%%======================================================================

generate_schema(App, AppDir, Singular, Plural, Fields) ->
    Mod = App ++ "_" ++ Singular,
    FileName = filename:join([AppDir, "src", "schemas", Mod ++ ".erl"]),
    Content = schema_content(Mod, Plural, Fields),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

schema_content(Mod, Table, Fields) ->
    FieldDefs = schema_fields(Fields),
    iolist_to_binary([
        "-module(",
        Mod,
        ").\n"
        "-behaviour(kura_schema).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([table/0, fields/0]).\n\n"
        "table() -> <<\"",
        Table,
        "\">>.\n\n"
        "fields() ->\n"
        "    [\n"
        "        #kura_field{name = id, type = id, primary_key = true, nullable = false}",
        FieldDefs,
        ",\n"
        "        #kura_field{name = inserted_at, type = utc_datetime},\n"
        "        #kura_field{name = updated_at, type = utc_datetime}\n"
        "    ].\n"
    ]).

schema_fields(Fields) ->
    iolist_to_binary([
        [",\n        #kura_field{name = ", Name, ", type = ", Type, "}"]
     || {Name, Type} <- Fields
    ]).

%%======================================================================
%% Internal: migration generator
%%======================================================================

generate_migration(AppDir, Table, Fields) ->
    TS = timestamp(),
    Mod = "m" ++ TS ++ "_create_" ++ Table,
    FileName = filename:join([AppDir, "src", "migrations", Mod ++ ".erl"]),
    Content = migration_content(Mod, Table, Fields),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

migration_content(Mod, Table, Fields) ->
    Columns = migration_columns(Fields),
    iolist_to_binary([
        "-module(",
        Mod,
        ").\n"
        "-behaviour(kura_migration).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([up/0, down/0]).\n\n"
        "up() ->\n"
        "    [{create_table, <<\"",
        Table,
        "\">>, [\n"
        "        #kura_column{name = id, type = id, primary_key = true}",
        Columns,
        ",\n"
        "        #kura_column{name = inserted_at, type = utc_datetime},\n"
        "        #kura_column{name = updated_at, type = utc_datetime}\n"
        "    ]}].\n\n"
        "down() ->\n"
        "    [{drop_table, <<\"",
        Table,
        "\">>}].\n"
    ]).

migration_columns(Fields) ->
    iolist_to_binary([
        [",\n        #kura_column{name = ", Name, ", type = ", Type, "}"]
     || {Name, Type} <- Fields
    ]).

%%======================================================================
%% Internal: test generator
%%======================================================================

generate_test(App, AppDir, Singular, Plural, Fields) ->
    Mod = App ++ "_" ++ Singular ++ "_live_SUITE",
    FileName = filename:join([AppDir, "test", Mod ++ ".erl"]),
    Content = test_content(Mod, App, Singular, Plural, Fields),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

test_content(Mod, App, Singular, _Plural, _Fields) ->
    Schema = App ++ "_" ++ Singular,
    Repo = App ++ "_repo",
    iolist_to_binary([
        "-module(",
        Mod,
        ").\n\n"
        "-include_lib(\"common_test/include/ct.hrl\").\n"
        "-include_lib(\"stdlib/include/assert.hrl\").\n\n"
        "-export([all/0, init_per_suite/1, end_per_suite/1]).\n"
        "-export([list_test/1, get_test/1, create_test/1, update_test/1, delete_test/1]).\n\n"
        "all() ->\n"
        "    [list_test, get_test, create_test, update_test, delete_test].\n\n"
        "init_per_suite(Config) ->\n"
        "    {ok, _} = application:ensure_all_started(",
        App,
        "),\n"
        "    Config.\n\n"
        "end_per_suite(_Config) ->\n"
        "    ok.\n\n"
        "list_test(_Config) ->\n"
        "    {ok, Items} = kura_repo_worker:all(",
        Repo,
        ", kura_query:from(",
        Schema,
        ")),\n"
        "    ?assert(is_list(Items)).\n\n"
        "get_test(_Config) ->\n"
        "    %% TODO: Create a record first, then fetch it\n"
        "    ok.\n\n"
        "create_test(_Config) ->\n"
        "    %% TODO: Insert with kura_changeset:cast/4 + kura_repo_worker:insert/2\n"
        "    ok.\n\n"
        "update_test(_Config) ->\n"
        "    %% TODO: Create a record, then update it\n"
        "    ok.\n\n"
        "delete_test(_Config) ->\n"
        "    %% TODO: Create a record, then delete it\n"
        "    ok.\n"
    ]).

%%======================================================================
%% Internal: route hints
%%======================================================================

print_route_hints(AppName, Plural, Actions) ->
    Singular = singularize(Plural),
    rebar_api:info("~nAdd these routes to your router:~n", []),
    rebar_api:info("  %% ~s views", [capitalize(Singular)]),
    lists:foreach(
        fun(Action) -> print_route_hint(AppName, Singular, Plural, Action) end,
        Actions
    ).

print_route_hint(AppName, Singular, Plural, index) ->
    rebar_api:info(
        "  {<<\"/~s\">>, {~s_~s_index_view, mount}, #{methods => [get]}}",
        [Plural, AppName, Singular]
    );
print_route_hint(AppName, Singular, Plural, show) ->
    rebar_api:info(
        "  {<<\"/~s/:id\">>, {~s_~s_show_view, mount}, #{methods => [get]}}",
        [Plural, AppName, Singular]
    );
print_route_hint(AppName, Singular, Plural, new) ->
    rebar_api:info(
        "  {<<\"/~s/new\">>, {~s_~s_new_view, mount}, #{methods => [get]}}",
        [Plural, AppName, Singular]
    );
print_route_hint(AppName, Singular, Plural, edit) ->
    rebar_api:info(
        "  {<<\"/~s/:id/edit\">>, {~s_~s_edit_view, mount}, #{methods => [get]}}",
        [Plural, AppName, Singular]
    );
print_route_hint(_, _, _, _) ->
    ok.

%%======================================================================
%% Internal: helpers
%%======================================================================

parse_fields(Str) ->
    Pairs = string:tokens(Str, ","),
    [parse_field(string:trim(P)) || P <- Pairs].

parse_field(Pair) ->
    case string:tokens(Pair, ":") of
        [Name, Type] -> {string:trim(Name), string:trim(Type)};
        [Name] -> {string:trim(Name), "string"};
        _ -> erlang:error({bad_field_spec, Pair})
    end.

singularize(Name) ->
    case lists:reverse(Name) of
        [$s | Rest] -> lists:reverse(Rest);
        _ -> Name
    end.

pluralize(Name) ->
    case lists:last(Name) of
        $s -> Name;
        _ -> Name ++ "s"
    end.

capitalize([H | T]) when H >= $a, H =< $z ->
    [H - 32 | T];
capitalize(Other) ->
    Other.

timestamp() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    lists:flatten(
        io_lib:format(
            "~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
            [Y, Mo, D, H, Mi, S]
        )
    ).

field_names_list(Fields) ->
    Names = [Name || {Name, _Type} <- Fields],
    "[" ++ string:join(Names, ", ") ++ "]".
