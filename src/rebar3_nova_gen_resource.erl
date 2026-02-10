-module(rebar3_nova_gen_resource).

-export([init/1, do/1, format_error/1]).

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
                {actions, $a, "actions", {string, "list,show,create,update,delete"}, "Comma-separated actions"}
            ]},
            {short_desc, "Generate controller, schema, and route hints"},
            {desc, "Generates a controller, JSON schema, and prints route snippets to add to your router"}
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
    Schema = #{
        <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"id">> => #{<<"type">> => <<"integer">>},
            <<"name">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"id">>, <<"name">>]
    },
    Json = thoas:encode(Schema),
    rebar3_nova_utils:write_file_if_not_exists(SchemaFile, Json).

print_route_hints(AppName, Name, Actions) ->
    Controller = io_lib:format("~s_~s_controller", [AppName, Name]),
    rebar_api:info("~nAdd these routes to your router:~n", []),
    lists:foreach(fun(list) ->
        rebar_api:info("  {<<\"/~s\">>, {~s, list}, #{methods => [get]}}", [Name, Controller]);
    (show) ->
        rebar_api:info("  {<<\"/~s/:id\">>, {~s, show}, #{methods => [get]}}", [Name, Controller]);
    (create) ->
        rebar_api:info("  {<<\"/~s\">>, {~s, create}, #{methods => [post]}}", [Name, Controller]);
    (update) ->
        rebar_api:info("  {<<\"/~s/:id\">>, {~s, update}, #{methods => [put]}}", [Name, Controller]);
    (delete) ->
        rebar_api:info("  {<<\"/~s/:id\">>, {~s, delete}, #{methods => [delete]}}", [Name, Controller]);
    (_) ->
        ok
    end, Actions).

singularize(Name) ->
    case lists:reverse(Name) of
        [$s | Rest] -> lists:reverse(Rest);
        _ -> Name
    end.
