-module(rebar3_nova_gen_controller).

-export([init/1, do/1, format_error/1]).
-export([generate/4, generate_content/2]).

-define(PROVIDER, gen_controller).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova gen_controller --name users"},
        {opts, [
            {name, $n, "name", string, "Controller name (required)"},
            {actions, $a, "actions", {string, "list,show,create,update,delete"},
                "Comma-separated actions"}
        ]},
        {short_desc, "Generate a Nova controller"},
        {desc, "Generates a controller module with stub action functions"}
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
            generate(AppName, AppDir, Name, Actions),
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec generate(atom(), file:filename(), string(), [atom()]) -> ok | skipped.
generate(AppName, AppDir, Name, Actions) ->
    ModName = io_lib:format("~s_~s_controller", [AppName, Name]),
    FileName = filename:join([AppDir, "src", "controllers", ModName ++ ".erl"]),
    Content = generate_content(erlang:list_to_atom(lists:flatten(ModName)), Actions),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

-doc "The source of a controller with a stub per action, formatted as erlfmt would.".
-spec generate_content(atom(), [atom()]) -> binary().
generate_content(ModName, Actions) ->
    Exports = lists:join(",\n", ["    " ++ atom_to_list(A) ++ "/1" || A <- Actions]),
    Header = ["-module(", atom_to_list(ModName), ").\n\n-export([\n", Exports, "\n]).\n"],
    unicode:characters_to_binary([Header | ["\n" ++ generate_function(A) || A <- Actions]]).

generate_function(create) ->
    "create(_Req) ->\n"
    "    {json, 201, #{}, #{~\"message\" => ~\"TODO\"}}.\n";
generate_function(delete) ->
    "delete(_Req) ->\n"
    "    {status, 204}.\n";
generate_function(Action) ->
    atom_to_list(Action) ++
        "(_Req) ->\n"
        "    {json, #{~\"message\" => ~\"TODO\"}}.\n".
