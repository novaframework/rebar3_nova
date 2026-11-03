-module(rebar3_nova_openapi).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_dispatch_SUITE.
-export([collect_routes/1, collect_routes/2, build_spec/4, component_schema/1]).

-include("nova_router.hrl").

-define(PROVIDER, openapi).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova openapi --prefix /api"},
        {opts, [
            {output, $o, "output", string, "Output file path (default: priv/assets/openapi.json)"},
            {title, $t, "title", string, "API title (default: app name)"},
            {api_version, $v, "api-version", {string, "0.1.0"}, "API version"},
            {prefix, $p, "prefix", string,
                "Only document paths under this prefix, e.g. /api (default: every controller route)"}
        ]},
        {short_desc, "Generate OpenAPI 3.0.3 spec from routes"},
        {desc,
            "Generates an OpenAPI 3.0.3 JSON specification from the compiled Nova routes. "
            "Static files, error pages and inline funs are left out."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [Hd | _] = rebar_state:project_apps(State),
    AppName = erlang:binary_to_atom(rebar_app_info:name(Hd)),
    AppDir = rebar_app_info:dir(Hd),

    {Args, _} = rebar_state:command_parsed_args(State),
    Output =
        case proplists:get_value(output, Args) of
            undefined -> filename:join([AppDir, "priv", "assets", "openapi.json"]);
            O -> O
        end,
    Title =
        case proplists:get_value(title, Args) of
            undefined -> erlang:atom_to_list(AppName);
            T -> T
        end,
    ApiVersion = proplists:get_value(api_version, Args, "0.1.0"),
    Prefix = proplists:get_value(prefix, Args, ""),

    Dispatch = rebar3_nova_utils:compile_routes(State, AppName),
    Routes = collect_routes(Dispatch, unicode:characters_to_binary(Prefix)),

    Schemas = load_schemas(AppDir),

    Spec = build_spec(Title, ApiVersion, Routes, Schemas),

    ok = filelib:ensure_dir(Output),
    ok = file:write_file(Output, [json:format(Spec), $\n]),
    rebar_api:info("OpenAPI spec written to ~s", [Output]),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Route collection
%% ===================================================================
collect_routes(Dispatch) ->
    collect_routes(Dispatch, ~"").

collect_routes(Dispatch, Prefix) ->
    [
        Route
     || Route = {Path, _M, _Mod, _F, _E} <- lists:flatmap(
            fun classify_route/1, rebar3_nova_dispatch:routes(Dispatch)
        ),
        under_prefix(Path, Prefix)
    ].

under_prefix(_Path, ~"") ->
    true;
under_prefix(Path, Prefix) ->
    Trimmed = string:trim(Prefix, trailing, "/"),
    Path =:= Trimmed orelse string:prefix(Path, <<Trimmed/binary, "/">>) =/= nomatch.

classify_route({Path, Method, #nova_handler_value{extra_state = Extra} = Value}) ->
    case rebar3_nova_dispatch:handler(Value) of
        {controller, Module, Function} ->
            expand_methods(
                Method, rebar3_nova_dispatch:openapi_path(Path), Module, Function, Extra
            );
        _StaticErrorPageOrInlineFun ->
            []
    end;
classify_route({_Path, _Method, _CowboyHandler}) ->
    [].

expand_methods('_', Path, Module, Function, Extra) ->
    Methods = [~"get", ~"post", ~"put", ~"delete", ~"patch"],
    [{Path, M, Module, Function, Extra} || M <- Methods];
expand_methods(Method, Path, Module, Function, Extra) ->
    [{Path, rebar3_nova_dispatch:method_to_binary(Method), Module, Function, Extra}].

%% ===================================================================
%% Schema loading
%% ===================================================================
load_schemas(AppDir) ->
    SchemaDir = filename:join([AppDir, "priv", "schemas"]),
    Files = filelib:wildcard("*.json", unicode:characters_to_list(SchemaDir)),
    maps:from_list([Schema || File <- Files, {ok, Schema} <- [load_schema(SchemaDir, File)]]).

load_schema(SchemaDir, File) ->
    case file:read_file(filename:join(SchemaDir, File)) of
        {ok, Bin} ->
            try json:decode(Bin) of
                Decoded when is_map(Decoded) ->
                    Name = unicode:characters_to_binary(filename:basename(File, ".json")),
                    {ok, {Name, component_schema(Decoded)}};
                _ ->
                    rebar_api:warn("Schema is not a JSON object: ~s", [File]),
                    error
            catch
                error:_ ->
                    rebar_api:warn("Failed to parse schema: ~s", [File]),
                    error
            end;
        {error, _} ->
            error
    end.

-doc """
A JSON Schema made fit for `components/schemas`. An OpenAPI 3.0 Schema
Object has no `$schema` or `$id` keywords, nor the draft-04 `id`.
""".
-spec component_schema(map()) -> map().
component_schema(Schema) ->
    Stripped = maps:without([~"$schema", ~"$id"], Schema),
    case Stripped of
        #{~"id" := Id} when is_binary(Id) -> maps:remove(~"id", Stripped);
        _ -> Stripped
    end.

%% ===================================================================
%% OpenAPI spec building
%% ===================================================================
build_spec(Title, Version, Routes, Schemas) ->
    Spec = #{
        ~"openapi" => ~"3.0.3",
        ~"info" => #{
            ~"title" => unicode:characters_to_binary(Title),
            ~"version" => unicode:characters_to_binary(Version)
        },
        ~"paths" => build_paths(Routes)
    },
    case maps:size(Schemas) of
        0 -> Spec;
        _ -> Spec#{~"components" => #{~"schemas" => Schemas}}
    end.

build_paths(Routes) ->
    BaseIds = [operation_id(Module, Function) || {_, _, Module, Function, _} <- Routes],
    Shared = BaseIds -- lists:usort(BaseIds),
    Operations = [
        {Path, Method, operation(Route, unique_id(Route, Shared))}
     || {Path, Method, _, _, _} = Route <- Routes
    ],
    Paths = lists:usort([Path || {Path, _, _} <- Operations]),
    maps:from_list([
        {Path, maps:from_list([{M, Op} || {P, M, Op} <- Operations, P =:= Path])}
     || Path <- Paths
    ]).

operation_id(Module, Function) ->
    <<(atom_to_binary(Module))/binary, ".", (atom_to_binary(Function))/binary>>.

%% operationId must be unique, so a handler serving several routes gets the
%% method and path appended.
unique_id({Path, Method, Module, Function, _Extra}, Shared) ->
    Base = operation_id(Module, Function),
    case lists:member(Base, Shared) of
        true -> <<Base/binary, ":", Method/binary, ":", Path/binary>>;
        false -> Base
    end.

operation({Path, Method, _Module, _Function, Extra}, OpId) ->
    Params = extract_path_params(Path),
    Body = request_body(Method, schema_ref(Extra)),
    Operation0 = #{
        ~"operationId" => OpId,
        ~"responses" => responses(Method, Body =/= none, Params =/= [])
    },
    Operation1 =
        case Params of
            [] -> Operation0;
            _ -> Operation0#{~"parameters" => Params}
        end,
    case Body of
        none -> Operation1;
        _ -> Operation1#{~"requestBody" => Body}
    end.

schema_ref(#{json_schema := SchemaPath}) ->
    Basename = filename:basename(SchemaPath, ".json"),
    Name = unicode:characters_to_binary(Basename),
    {ok, <<"#/components/schemas/", Name/binary>>};
schema_ref(_) ->
    none.

request_body(Method, {ok, Ref}) when
    Method =:= ~"post"; Method =:= ~"put"; Method =:= ~"patch"
->
    #{
        ~"required" => true,
        ~"content" => #{~"application/json" => #{~"schema" => #{~"$ref" => Ref}}}
    };
request_body(_Method, _SchemaRef) ->
    none.

responses(Method, HasBody, HasParams) ->
    Success = success_response(Method),
    WithBody =
        case HasBody of
            true -> Success#{~"400" => #{~"description" => ~"Invalid request body"}};
            false -> Success
        end,
    case HasParams of
        true -> WithBody#{~"404" => #{~"description" => ~"Not found"}};
        false -> WithBody
    end.

success_response(~"post") -> #{~"201" => #{~"description" => ~"Created"}};
success_response(~"delete") -> #{~"204" => #{~"description" => ~"No content"}};
success_response(_Method) -> #{~"200" => #{~"description" => ~"Successful response"}}.

extract_path_params(Path) ->
    [
        #{
            ~"name" => binary:part(Rest, 0, byte_size(Rest) - 1),
            ~"in" => ~"path",
            ~"required" => true,
            ~"schema" => #{~"type" => ~"string"}
        }
     || <<"{", Rest/binary>> <- binary:split(Path, ~"/", [global])
    ].
