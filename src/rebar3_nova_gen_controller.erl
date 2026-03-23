-module(rebar3_nova_gen_controller).

-export([init/1, do/1, run/1, run/2, format_error/1]).
-export([generate/4]).

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
        {short_desc, "Generate a Nova controller (deprecated: use `rebar3 nova gen controller`)"},
        {desc, "Generates a controller module with stub action functions"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:warn(
        "gen_controller is deprecated. Use `rebar3 nova gen controller` instead.", []
    ),
    run(State).

-spec run(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
run(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    run(State, Args).

-spec run(rebar_state:t(), proplists:proplist()) -> {ok, rebar_state:t()} | {error, string()}.
run(State, Args) ->
    case proplists:get_value(name, Args) of
        undefined ->
            rebar_api:abort(
                "Name is required.~n~n"
                "Usage: rebar3 nova gen controller <name> [options]~n~n"
                "Options:~n"
                "  --actions  Comma-separated actions (default: list,show,create,update,delete)~n~n"
                "Example:~n"
                "  rebar3 nova gen controller users~n"
                "  rebar3 nova gen controller posts --actions index,show",
                []
            );
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
    ModName = lists:flatten(io_lib:format("~s_~s_controller", [AppName, Name])),
    FileName = filename:join([AppDir, "src", "controllers", ModName ++ ".erl"]),
    Context = #{
        mod => ModName,
        exports => format_exports(Actions),
        action_functions => [#{code => generate_function(A)} || A <- Actions]
    },
    Content = rebar3_nova_utils:render_template(["gen", "controller.erl.mustache"], Context),
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

format_exports(Actions) ->
    string:join([io_lib:format("~s/1", [A]) || A <- Actions], ", ").

generate_function(list) ->
    "list(#{req := _Req} = _NovaReq) ->\n"
    "    {json, #{<<\"message\">> => <<\"TODO\">>}}.\n";
generate_function(show) ->
    "show(#{req := _Req} = _NovaReq) ->\n"
    "    {json, #{<<\"message\">> => <<\"TODO\">>}}.\n";
generate_function(create) ->
    "create(#{req := _Req} = _NovaReq) ->\n"
    "    {status, 201, #{}, #{<<\"message\">> => <<\"TODO\">>}}.\n";
generate_function(update) ->
    "update(#{req := _Req} = _NovaReq) ->\n"
    "    {json, #{<<\"message\">> => <<\"TODO\">>}}.\n";
generate_function(delete) ->
    "delete(#{req := _Req} = _NovaReq) ->\n"
    "    {status, 204}.\n";
generate_function(Action) ->
    lists:flatten(
        io_lib:format(
            "~s(#{req := _Req} = _NovaReq) ->~n"
            "    {json, #{<<\"message\">> => <<\"TODO\">>}}.~n",
            [Action]
        )
    ).
