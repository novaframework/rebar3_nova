-module(rebar3_nova_gen_resource).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_gen_SUITE.
-export([schema/0, route_hints/3]).

-define(PROVIDER, gen_resource).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova gen_resource --name users"},
        {opts, [
            {name, $n, "name", string, "Resource name (required)"},
            {actions, $a, "actions", {string, "list,show,create,update,delete"},
                "Comma-separated actions"}
        ]},
        {short_desc, "Generate controller, schema, and route hints"},
        {desc,
            "Generates a controller, JSON schema, and prints route snippets to add to your router"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(name, Args) of
        undefined ->
            rebar_api:abort("--name is required", []);
        Name ->
            AppName = rebar3_nova_utils:get_app_name(State),
            AppDir = rebar3_nova_utils:get_app_dir(State),
            ActionsStr = proplists:get_value(actions, Args, "list,show,create,update,delete"),
            Actions = rebar3_nova_utils:parse_actions(ActionsStr),
            rebar3_nova_gen_controller:generate(AppName, AppDir, Name, Actions),
            generate_schema(AppDir, Name),
            print_route_hints(AppName, Name, Actions),
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

generate_schema(AppDir, Name) ->
    Singular = singularize(Name),
    SchemaFile = filename:join([AppDir, "priv", "schemas", Singular ++ ".json"]),
    rebar3_nova_utils:write_file_if_not_exists(SchemaFile, [json:format(schema()), $\n]).

-doc """
A starting request-body schema. Draft 6, because jesse (behind
nova_json_schemas) knows drafts 3, 4 and 6 only; `id` is left out of
`required` since a create body has none.
""".
-spec schema() -> map().
schema() ->
    #{
        ~"$schema" => ~"http://json-schema.org/draft-06/schema#",
        ~"type" => ~"object",
        ~"properties" => #{
            ~"name" => #{~"type" => ~"string"}
        },
        ~"required" => [~"name"]
    }.

print_route_hints(AppName, Name, Actions) ->
    rebar_api:info("~nAdd these route groups to your router:~n", []),
    lists:foreach(
        fun(Line) -> rebar_api:info("  ~ts", [Line]) end,
        string:split(route_hints(AppName, Name, Actions), "\n", all)
    ).

-doc """
Route groups for the actions: reads in one, and create, update and delete in
another behind a security function, so the writes are not public by default.
The write group lists its own plugins, with a body size limit before the
body decoder.
""".
-spec route_hints(atom(), string(), [atom()]) -> string().
route_hints(AppName, Name, Actions) ->
    Controller = lists:flatten(io_lib:format("~s_~s_controller", [AppName, Name])),
    Schema = "schemas/" ++ singularize(Name) ++ ".json",
    Routes = fun(Wanted) ->
        [
            Hint
         || Action <- Actions,
            lists:member(Action, Wanted),
            Hint <- route_hint(Action, Name, Controller, Schema)
        ]
    end,
    Writes = Routes([create, update, delete]),
    Protected = io_lib:format(
        "  %% Placeholders: `rebar3 nova gen_auth` generates require_bearer/1 and\n"
        "  %% ~s_body_limit; otherwise use your own security function and a\n"
        "  %% plugin that refuses large bodies. Until both exist, these routes\n"
        "  %% fail closed.\n"
        "  security => fun ~s_auth_security:require_bearer/1,\n"
        "  %% Plugins run before security, so the group sets its own instead of\n"
        "  %% inheriting a global body decoder that reads any size of body.\n"
        "  plugins => [\n"
        "      {pre_request, ~s_body_limit, #{}},\n"
        "      {pre_request, nova_request_plugin, #{decode_json_body => true}}~ts\n"
        "  ],\n",
        [AppName, AppName, AppName, json_schemas_plugin(Writes)]
    ),
    Groups =
        [route_group("", Reads) || Reads <- [Routes([list, show])], Reads =/= []] ++
            [route_group(Protected, Writes) || Writes =/= []],
    lists:flatten(io_lib:format("~ts", [lists:join(",\n", Groups)])).

%% A group's own plugins replace the global ones, so the json_schema in
%% extra_state is only checked if nova_json_schemas is listed again.
json_schemas_plugin(Routes) ->
    case [Route || Route <- Routes, string:find(Route, "json_schema") =/= nomatch] of
        [] -> "";
        _ -> ",\n      {pre_request, nova_json_schemas, #{}}"
    end.

route_group(Security, Routes) ->
    [
        "#{prefix => \"\",\n",
        Security,
        "  routes => [\n",
        lists:join(",\n", ["      " ++ Route || Route <- Routes]),
        "\n  ]}"
    ].

route_hint(list, Name, Controller, _Schema) ->
    hint("/" ++ Name, Controller, list, "get", "");
route_hint(show, Name, Controller, _Schema) ->
    hint("/" ++ Name ++ "/:id", Controller, show, "get", "");
route_hint(create, Name, Controller, Schema) ->
    hint("/" ++ Name, Controller, create, "post", schema_option(Schema));
route_hint(update, Name, Controller, Schema) ->
    hint("/" ++ Name ++ "/:id", Controller, update, "put", schema_option(Schema));
route_hint(delete, Name, Controller, _Schema) ->
    hint("/" ++ Name ++ "/:id", Controller, delete, "delete", "");
route_hint(_Action, _Name, _Controller, _Schema) ->
    [].

hint(Path, Controller, Function, Method, Extra) ->
    [
        lists:flatten(
            io_lib:format("{\"~ts\", fun ~ts:~ts/1, #{methods => [~ts]~ts}}", [
                Path, Controller, Function, Method, Extra
            ])
        )
    ].

schema_option(Schema) ->
    ", extra_state => #{json_schema => \"" ++ Schema ++ "\"}".

singularize(Name) ->
    case lists:reverse(Name) of
        [$s | Rest] -> lists:reverse(Rest);
        _ -> Name
    end.
