-module(rebar3_nova_openapi).

-export([init/1, do/1, format_error/1]).

-include("nova_router.hrl").
-include_lib("routing_tree/include/routing_tree.hrl").

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
            {example, "rebar3 nova openapi"},
            {opts, [
                {output, $o, "output", string, "Output file path (default: priv/assets/openapi.json)"},
                {title, $t, "title", string, "API title (default: app name)"},
                {api_version, $v, "api-version", {string, "0.1.0"}, "API version"}
            ]},
            {short_desc, "Generate OpenAPI 3.0.3 spec from routes"},
            {desc, "Generates an OpenAPI 3.0.3 JSON specification from compiled Nova routes"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [Hd|_] = rebar_state:project_apps(State),
    AppName = erlang:binary_to_atom(rebar_app_info:name(Hd)),
    AppDir = rebar_app_info:dir(Hd),

    {Args, _} = rebar_state:command_parsed_args(State),
    Output = case proplists:get_value(output, Args) of
                 undefined -> filename:join([AppDir, "priv", "assets", "openapi.json"]);
                 O -> O
             end,
    Title = case proplists:get_value(title, Args) of
                undefined -> erlang:atom_to_list(AppName);
                T -> T
            end,
    ApiVersion = proplists:get_value(api_version, Args, "0.1.0"),

    Dispatch = nova_router:compile([AppName]),
    Routes = collect_routes(Dispatch),

    Schemas = load_schemas(AppDir),

    Spec = build_spec(Title, ApiVersion, Routes, Schemas),
    Json = thoas:encode(Spec),

    ok = filelib:ensure_dir(Output),
    ok = file:write_file(Output, Json),
    rebar_api:info("OpenAPI spec written to ~s", [Output]),

    SwaggerPath = filename:join(filename:dirname(Output), "swagger.html"),
    ok = file:write_file(SwaggerPath, swagger_html(Output)),
    rebar_api:info("Swagger UI written to ~s", [SwaggerPath]),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Route collection
%% ===================================================================
collect_routes(#host_tree{hosts = Hosts}) ->
    lists:flatmap(fun({_Host, #routing_tree{tree = Tree}}) ->
        collect_nodes(Tree, <<>>)
    end, Hosts).

collect_nodes([], _Prefix) -> [];
collect_nodes([#node{is_wildcard = true}|Tl], Prefix) ->
    collect_nodes(Tl, Prefix);
collect_nodes([#node{segment = Segment}|Tl], Prefix) when is_integer(Segment) ->
    collect_nodes(Tl, Prefix);
collect_nodes([#node{segment = Segment, is_binding = IsBinding, value = Value, children = Children}|Tl], Prefix) ->
    SegBin = segment_to_binary(Segment, IsBinding),
    NewPrefix = <<Prefix/binary, "/", SegBin/binary>>,
    HandlerRoutes = lists:filtermap(fun(NodeComp) -> classify_handler(NodeComp, NewPrefix) end, Value),
    HandlerRoutes ++ collect_nodes(Children, NewPrefix) ++ collect_nodes(Tl, Prefix).

segment_to_binary(Segment, true) when is_binary(Segment) ->
    <<"{", Segment/binary, "}">>;
segment_to_binary(Segment, _) when is_binary(Segment) ->
    Segment;
segment_to_binary(Segment, IsBinding) when is_list(Segment) ->
    segment_to_binary(erlang:list_to_binary(Segment), IsBinding).

%% ===================================================================
%% Handler classification
%% ===================================================================
classify_handler(#node_comp{value = #nova_handler_value{module = nova_file_controller}}, _Path) ->
    false;
classify_handler(#node_comp{value = #nova_handler_value{module = nova_error_controller}}, _Path) ->
    false;
classify_handler(#node_comp{comparator = Method,
                            value = #nova_handler_value{module = undefined, function = undefined,
                                                        callback = Callback, extra_state = Extra}}, Path) ->
    {module, Module} = lists:keyfind(module, 1, erlang:fun_info(Callback)),
    {name, Function} = lists:keyfind(name, 1, erlang:fun_info(Callback)),
    expand_methods(Method, Path, Module, Function, Extra);
classify_handler(#node_comp{comparator = Method,
                            value = #nova_handler_value{module = Module, function = Function,
                                                        extra_state = Extra}}, Path) ->
    expand_methods(Method, Path, Module, Function, Extra);
classify_handler(#node_comp{value = #cowboy_handler_value{}}, _Path) ->
    false.

expand_methods('_', Path, Module, Function, Extra) ->
    Methods = [<<"get">>, <<"post">>, <<"put">>, <<"delete">>, <<"patch">>],
    {true, [{Path, M, Module, Function, Extra} || M <- Methods]};
expand_methods(Method, Path, Module, Function, Extra) ->
    {true, [{Path, method_to_binary(Method), Module, Function, Extra}]}.

method_to_binary(Method) when is_atom(Method) ->
    erlang:atom_to_binary(Method);
method_to_binary(Method) when is_binary(Method) ->
    string:lowercase(Method).

%% ===================================================================
%% Schema loading
%% ===================================================================
load_schemas(AppDir) ->
    SchemaDir = filename:join([AppDir, "priv", "schemas"]),
    SchemaDirStr = unicode:characters_to_list(SchemaDir),
    case filelib:is_dir(SchemaDirStr) of
        false -> #{};
        true ->
            Files = filelib:wildcard("*.json", SchemaDirStr),
            maps:from_list(lists:filtermap(fun(File) ->
                FullPath = filename:join(SchemaDir, File),
                case file:read_file(FullPath) of
                    {ok, Bin} ->
                        case thoas:decode(Bin) of
                            {ok, Decoded} ->
                                Name = erlang:list_to_binary(filename:basename(File, ".json")),
                                {true, {Name, Decoded}};
                            {error, _} ->
                                rebar_api:warn("Failed to parse schema: ~s", [File]),
                                false
                        end;
                    {error, _} ->
                        false
                end
            end, Files))
    end.

%% ===================================================================
%% OpenAPI spec building
%% ===================================================================
build_spec(Title, Version, Routes, Schemas) ->
    Paths = build_paths(Routes),
    Spec = #{
        <<"openapi">> => <<"3.0.3">>,
        <<"info">> => #{
            <<"title">> => erlang:list_to_binary(Title),
            <<"version">> => erlang:list_to_binary(Version)
        },
        <<"paths">> => Paths
    },
    case maps:size(Schemas) of
        0 -> Spec;
        _ -> Spec#{<<"components">> => #{<<"schemas">> => Schemas}}
    end.

build_paths(Routes) ->
    FlatRoutes = lists:flatten(Routes),
    Grouped = lists:foldl(fun({Path, Method, Module, Function, Extra}, Acc) ->
        PathMethods = maps:get(Path, Acc, #{}),
        OpId = <<(erlang:atom_to_binary(Module))/binary, ".", (erlang:atom_to_binary(Function))/binary>>,
        Params = extract_path_params(Path),
        SchemaRef = schema_ref(Extra),
        Operation0 = #{<<"operationId">> => OpId},
        Operation1 = case Params of
            [] -> Operation0;
            _ -> Operation0#{<<"parameters">> => Params}
        end,
        Operation2 = maybe_add_request_body(Method, SchemaRef, Operation1),
        Operation3 = maybe_add_response_schema(Method, SchemaRef, Operation2),
        Acc#{Path => PathMethods#{Method => Operation3}}
    end, #{}, FlatRoutes),
    Grouped.

schema_ref(#{json_schema := SchemaPath}) ->
    Basename = filename:basename(SchemaPath, ".json"),
    Name = unicode:characters_to_binary(Basename),
    {ok, <<"#/components/schemas/", Name/binary>>};
schema_ref(_) ->
    none.

maybe_add_request_body(Method, {ok, Ref}, Operation)
  when Method =:= <<"post">>; Method =:= <<"put">>; Method =:= <<"patch">> ->
    Operation#{<<"requestBody">> => #{
        <<"required">> => true,
        <<"content">> => #{
            <<"application/json">> => #{
                <<"schema">> => #{<<"$ref">> => Ref}
            }
        }
    }};
maybe_add_request_body(_Method, _SchemaRef, Operation) ->
    Operation.

maybe_add_response_schema(_Method, {ok, Ref}, Operation) ->
    Operation#{<<"responses">> => #{
        <<"200">> => #{
            <<"description">> => <<"Successful response">>,
            <<"content">> => #{
                <<"application/json">> => #{
                    <<"schema">> => #{<<"$ref">> => Ref}
                }
            }
        }
    }};
maybe_add_response_schema(_Method, none, Operation) ->
    Operation#{<<"responses">> => #{
        <<"200">> => #{<<"description">> => <<"Successful response">>}
    }}.

extract_path_params(Path) ->
    Segments = binary:split(Path, <<"/">>, [global]),
    lists:filtermap(fun(Segment) ->
        case Segment of
            <<"{", Rest/binary>> ->
                Name = binary:part(Rest, 0, byte_size(Rest) - 1),
                {true, #{
                    <<"name">> => Name,
                    <<"in">> => <<"path">>,
                    <<"required">> => true,
                    <<"schema">> => #{<<"type">> => <<"string">>}
                }};
            _ ->
                false
        end
    end, Segments).

%% ===================================================================
%% Swagger UI HTML
%% ===================================================================
swagger_html(SpecPath) ->
    SpecFile = filename:basename(SpecPath),
    ["<!DOCTYPE html>\n"
     "<html lang=\"en\">\n"
     "<head>\n"
     "  <meta charset=\"UTF-8\">\n"
     "  <title>Swagger UI</title>\n"
     "  <link rel=\"stylesheet\" href=\"https://unpkg.com/swagger-ui-dist@5/swagger-ui.css\">\n"
     "</head>\n"
     "<body>\n"
     "  <div id=\"swagger-ui\"></div>\n"
     "  <script src=\"https://unpkg.com/swagger-ui-dist@5/swagger-ui-bundle.js\"></script>\n"
     "  <script>\n"
     "    SwaggerUIBundle({\n"
     "      url: '", SpecFile, "',\n"
     "      dom_id: '#swagger-ui',\n"
     "      presets: [SwaggerUIBundle.presets.apis],\n"
     "      layout: 'BaseLayout'\n"
     "    });\n"
     "  </script>\n"
     "</body>\n"
     "</html>\n"].
