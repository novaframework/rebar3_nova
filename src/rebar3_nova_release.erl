-module(rebar3_nova_release).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, release).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {namespace, nova},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 nova release --profile prod"},
            {opts, [
                {profile, $p, "profile", {string, "prod"}, "Release profile to use"}
            ]},
            {short_desc, "Build a Nova release"},
            {desc, "Regenerates OpenAPI spec if schemas exist, then builds a release"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    AppDir = rebar3_nova_utils:get_app_dir(State),
    {Args, _} = rebar_state:command_parsed_args(State),
    Profile = proplists:get_value(profile, Args, "prod"),

    SchemaDir = filename:join([AppDir, "priv", "schemas"]),
    case filelib:is_dir(SchemaDir) of
        true ->
            OutputPath = filename:join([AppDir, "priv", "assets", "openapi.json"]),
            rebar3_nova_utils:ensure_dir(OutputPath),
            rebar_api:info("Regenerating OpenAPI spec to ~s", [OutputPath]),
            rebar3_nova_openapi:do(State);
        false ->
            ok
    end,

    ProfileAtom = erlang:list_to_atom(Profile),
    Providers = rebar_state:providers(State),
    case providers:get_provider(release, Providers, default) of
        not_found ->
            rebar_api:abort("Release provider not found. Is relx configured?", []);
        ReleaseProvider ->
            State1 = rebar_state:current_profiles(State, [ProfileAtom]),
            case providers:do(ReleaseProvider, State1) of
                {ok, State2} ->
                    rebar_api:info("Release built successfully with profile '~s'", [Profile]),
                    {ok, State2};
                {error, Err} ->
                    rebar_api:abort("Release failed: ~p", [Err])
            end
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
