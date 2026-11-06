-module({{name}}_repo).
-behaviour(kura_repo).

%% Migrations do not run at boot, where nodes starting together would race
%% on them. Run them as a release step, once per deploy, on the running node:
%%   bin/{{name}} eval 'kura_migrator:migrate({{name}}_repo).'
%% In development, call kura_migrator:migrate({{name}}_repo) in rebar3 shell.

-export([otp_app/0]).

otp_app() -> {{name}}.
