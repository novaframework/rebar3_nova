-module(rebar3_nova_gen).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, nova},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 nova gen controller users"},
        {opts, [
            {actions, $a, "actions", string, "Comma-separated actions"},
            {fields, $f, "fields", string, "Comma-separated field:type pairs"},
            {no_schema, undefined, "no-schema", boolean, "Skip schema/migration generation"}
        ]},
        {short_desc, "Generate code (controller, resource, test, auth, live)"},
        {desc,
            "Unified code generator.\n\n"
            "Usage:\n"
            "  rebar3 nova gen controller users\n"
            "  rebar3 nova gen resource users\n"
            "  rebar3 nova gen test users\n"
            "  rebar3 nova gen auth\n"
            "  rebar3 nova gen live users --fields name:string,email:string\n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, Args} = rebar_state:command_parsed_args(State),
    case Args of
        [Type | NameArgs] ->
            Name =
                case NameArgs of
                    [N | _] -> N;
                    [] -> undefined
                end,
            dispatch(Type, State, [{name, Name} | Opts]);
        [] ->
            rebar_api:abort(
                "Missing generator type. Usage: rebar3 nova gen <type> <name>~n"
                "Available types: ~s",
                [string:join(maps:keys(generators()), ", ")]
            )
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%----------------------------------------------------------------------
%% Internal
%%----------------------------------------------------------------------

dispatch(Type, State, MergedOpts) ->
    case maps:find(Type, generators()) of
        {ok, Module} ->
            Module:run(State, MergedOpts);
        error ->
            rebar_api:abort(
                "Unknown generator type: ~s~n"
                "Available types: ~s",
                [Type, string:join(maps:keys(generators()), ", ")]
            )
    end.

generators() ->
    #{
        "controller" => rebar3_nova_gen_controller,
        "resource" => rebar3_nova_gen_resource,
        "test" => rebar3_nova_gen_test,
        "auth" => rebar3_nova_gen_auth,
        "live" => rebar3_nova_gen_live
    }.
