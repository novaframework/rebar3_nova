-module(rebar3_nova_test_settings).

%% Stands in for the nova_settings of a Nova that checks its settings at
%% boot, so the suites say the same on a Nova that has none.

-export([checked/2, expected/1, removed/0]).

-define(SECRET, ~"a secret that only the test suites use 0123456789").

checked(trusted_origins, Value) when Value =:= same_host; Value =:= same_origin; Value =:= any ->
    {ok, Value};
checked(csrf_token, Value) when Value =:= undefined; Value =:= stateless; Value =:= session ->
    {ok, Value};
checked(non_canonical_paths, Value) when Value =:= reject; Value =:= resolve ->
    {ok, Value};
checked(max_sessions, Value) when is_integer(Value), Value > 0 ->
    {ok, Value};
checked(plugins_before_security, Value) when Value =:= all; Value =:= [] ->
    {ok, Value};
checked(secret_key_base, undefined) ->
    {ok, []};
checked(secret_key_base, ?SECRET) ->
    {ok, [~"a key made from the secret"]};
checked(secret_key_base, _Refused) ->
    {error, redacted};
checked(_Key, Value) ->
    {error, Value}.

expected(Key) ->
    <<(atom_to_binary(Key))/binary, " must be something else">>.

removed() ->
    [{csrf_secret, secret_key_base}, {allowed_origins, trusted_origins}].
