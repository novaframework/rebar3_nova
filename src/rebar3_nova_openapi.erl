-module(rebar3_nova_openapi).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_dispatch_SUITE.
-export([
    collect_routes/1, collect_routes/2,
    build_spec/4,
    component_schema/1,
    component_names/1,
    load_schemas/1,
    default_output/1
]).

-include("nova_router.hrl").
-include_lib("kernel/include/file.hrl").

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
            {output, $o, "output", string,
                "Output file path (default: priv/assets/openapi.json, served under /assets)"},
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
            undefined -> default_output(AppDir);
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
    ok = rebar3_nova_dispatch:warn_unknown(Dispatch),
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

-doc """
Where the spec goes without `--output`: `priv/assets`, which the scaffold
serves under `/assets`. Pass `--output` to keep it private.
""".
-spec default_output(file:filename_all()) -> file:filename_all().
default_output(AppDir) ->
    filename:join([AppDir, "priv", "assets", "openapi.json"]).

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
    Files = schema_files(unicode:characters_to_list(SchemaDir)),
    case component_names(Files) of
        {ok, Named} ->
            maps:from_list([
                {Name, Schema}
             || {Name, File} <- Named, {ok, Schema} <- [load_schema(SchemaDir, File)]
            ]);
        {error, {collision, Name, Paths}} ->
            rebar_api:abort("Schemas ~p all map to the component name ~ts; rename one", [
                Paths, Name
            ])
    end.

-doc """
Component names for schema files, given relative to `priv/schemas` as
nova_json_schemas keys them: `v1/user.json` becomes `v1.user`. Characters
an OpenAPI component name cannot hold become `_`; two files that end up
with the same name are an error rather than one silently replacing the
other.
""".
-spec component_names([file:filename()]) ->
    {ok, [{binary(), file:filename()}]} | {error, {collision, binary(), [file:filename()]}}.
component_names(Files) ->
    Named = lists:sort([{component_name(File), File} || File <- Files]),
    case [Name || {Name, _} <- Named] -- lists:usort([Name || {Name, _} <- Named]) of
        [] ->
            {ok, Named};
        [Name | _] ->
            {error, {collision, Name, [File || {N, File} <- Named, N =:= Name]}}
    end.

component_name(RelPath) ->
    Segments = filename:split(filename:rootname(RelPath, ".json")),
    Joined = unicode:characters_to_binary(lists:join(".", Segments)),
    re:replace(Joined, "[^A-Za-z0-9._-]", "_", [global, unicode, {return, binary}]).

load_schema(SchemaDir, File) ->
    case file:read_file(filename:join(SchemaDir, File)) of
        {ok, Bin} ->
            try json:decode(Bin) of
                Decoded when is_map(Decoded) ->
                    {ok, component_schema(Decoded)};
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

%% Symlinks below priv/schemas are never followed: the spec may be
%% published, and a link could pull in any file on the machine.
schema_files(SchemaDir) ->
    lists:append([schema_files(SchemaDir, [Name]) || Name <- list_dir(SchemaDir)]).

schema_files(SchemaDir, RelPath) ->
    Path = filename:join([SchemaDir | RelPath]),
    RelName = filename:join(RelPath),
    case file:read_link_info(Path) of
        {ok, #file_info{type = directory}} ->
            lists:append([schema_files(SchemaDir, RelPath ++ [Name]) || Name <- list_dir(Path)]);
        {ok, #file_info{type = regular}} ->
            case filename:extension(RelName) of
                ".json" -> [RelName];
                _ -> []
            end;
        {ok, #file_info{type = symlink}} ->
            rebar_api:warn("Skipping ~ts under priv/schemas: it is a symlink", [RelName]),
            [];
        _ ->
            []
    end.

list_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Names} -> Names;
        {error, _} -> []
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
        ~"paths" => build_paths(Routes, Schemas)
    },
    case maps:size(Schemas) of
        0 -> Spec;
        _ -> Spec#{~"components" => #{~"schemas" => Schemas}}
    end.

build_paths(Routes, Schemas) ->
    BaseIds = [operation_id(Module, Function) || {_, _, Module, Function, _} <- Routes],
    Shared = BaseIds -- lists:usort(BaseIds),
    Operations = [
        {Path, Method, operation(Route, unique_id(Route, Shared), Schemas)}
     || {Path, Method, _, _, _} = Route <- Routes
    ],
    Paths = lists:usort([Path || {Path, _, _} <- Operations]),
    #{Path => #{M => Op || {P, M, Op} <- Operations, P =:= Path} || Path <- Paths}.

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

operation({Path, Method, _Module, _Function, Extra}, OpId, Schemas) ->
    Params = extract_path_params(Path),
    Body = request_body(Method, schema_ref(Extra, Schemas)),
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

%% json_schema names the request-body schema, as nova_json_schemas reads it:
%% a path such as "schemas/v1/user.json", relative to priv. A schema that
%% was not loaded gets no $ref, which would dangle.
schema_ref(#{json_schema := SchemaPath}, Schemas) when is_list(SchemaPath); is_binary(SchemaPath) ->
    Name = component_name(schema_relative_path(unicode:characters_to_list(SchemaPath))),
    case maps:is_key(Name, Schemas) of
        true -> {ok, <<"#/components/schemas/", Name/binary>>};
        false -> none
    end;
schema_ref(_Extra, _Schemas) ->
    none.

schema_relative_path("./" ++ Path) -> schema_relative_path(Path);
schema_relative_path("schemas/" ++ Path) -> Path;
schema_relative_path(Path) -> Path.

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
