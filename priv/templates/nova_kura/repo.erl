-module({{name}}_repo).
-behaviour(kura_repo).

-export([otp_app/0, init/1]).

otp_app() -> {{name}}.

init(Config) ->
    maybe_apply_database_url(Config).

maybe_apply_database_url(Config) ->
    case os:getenv("DATABASE_URL") of
        false ->
            Config;
        Url ->
            parse_database_url(Url, Config)
    end.

parse_database_url(Url, Config) ->
    case uri_string:parse(list_to_binary(Url)) of
        #{scheme := <<"postgres", _/binary>>,
          host := Host,
          path := Path,
          userinfo := UserInfo} = Parsed ->
            [User, Pass] = binary:split(UserInfo, <<":">>),
            Db = binary:part(Path, 1, byte_size(Path) - 1),
            Port = maps:get(port, Parsed, 5432),
            Config#{
                hostname => Host,
                port => Port,
                username => User,
                password => Pass,
                database => Db
            };
        _ ->
            Config
    end.
