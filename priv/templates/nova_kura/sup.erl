-module({{name}}_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
