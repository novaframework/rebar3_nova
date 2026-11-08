-module(rebar3_nova_release).

-export([init/1, do/1, format_error/1]).

%% Exported for rebar3_nova_commands_SUITE.
-export([as_args/1]).

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
        {desc, "Builds a release with `rebar3 as <profile> release`"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Profile = proplists:get_value(profile, Args, "prod"),
    rebar_api:info("Building release with profile '~s'", [Profile]),
    case rebar_prv_as:do(rebar_state:command_args(State, as_args(Profile))) of
        {ok, State1} ->
            rebar_api:info("Release built successfully with profile '~s'", [Profile]),
            {ok, State1};
        {error, Err} ->
            rebar_api:abort("Release failed: ~p", [Err])
    end.

-doc "Arguments for `rebar3 as`, so the release runs exactly like `rebar3 as Profile release`.".
-spec as_args(string()) -> [string()].
as_args(Profile) ->
    [Profile, "release"].

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
