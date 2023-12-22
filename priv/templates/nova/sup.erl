%%%-------------------------------------------------------------------
%% @doc {{name}} top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module({{name}}_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {{#use_bossdb}}
    %% Start boss_db
    Configuration = application:get_env(nova, database_config, #{}),
    setup_bossdb(Configuration),
    {{/use_bossdb}}
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
{{#use_bossdb}}
setup_bossdb(#{}) ->
    logger:warning(#{msg => "Could not start boss_db because of empty configuration"});
setup_bossdb(Configuration) ->
    logger:debug(#{msg => "Starting boss_db with config", config => Configuration}),
    boss_db:start(Configuration),
    boss_news:start().
{{/use_bossdb}}
