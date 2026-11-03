-module(rebar3_nova_gen_auth_SUITE).

%% Runs the gen_auth output: the generated plugins, security and controller
%% modules are compiled and served by Nova with the routes the instructions
%% print, on nova_auth 0.3. Only the accounts module is a stub, so no
%% database is needed. The generated sources are also checked as text.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kura/include/kura.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([
    bearer_scope_does_not_read_an_unauthenticated_body/1,
    public_scope_refuses_a_large_body_unread/1,
    public_scope_refuses_a_chunked_body_unread/1,
    public_scope_accepts_a_small_body/1,
    login_is_limited_per_email_and_address/1,
    login_is_limited_per_email_across_addresses/1,
    malformed_login_spends_no_attempts/1,
    invalid_utf8_authorization_is_unauthorized/1,
    auth_data_has_no_password_hash/1,
    bearer_token_is_bytewise/1,
    ipv6_addresses_share_a_bucket_per_64/1,
    rate_limit_key_uses_nova_auth_trusted_proxies/1,
    account_limiter_is_created_once/1,
    limiters_allow_5_per_address_and_50_per_email_in_5_minutes/1,
    limiters_key_on_a_hash_of_the_email/1,
    emails_over_254_bytes_are_not_counted/1,
    token_response_is_not_cached/1,
    registration_hides_database_errors/1,
    login_hides_token_errors/1,
    one_address_cannot_lock_an_account/1,
    retry_after_rounds_up/1,
    body_limit_reply_has_no_connection_header/1,
    bearer_token_allows_only_trailing_padding/1,
    files_are_valid_formatted_erlang/1,
    uses_nova_auth_not_bcrypt_or_thoas/1,
    drops_the_nova_auth_0_2_workarounds/1,
    writes_no_migration/1,
    names_follow_the_app/1,
    rate_limit_runs_before_the_body_is_decoded/1,
    protected_group_example_limits_the_body/1,
    suite_builds_binaries_with_sigils/1,
    token_schema_has_the_refresh_fields_and_indexes/1,
    registration_needs_email_and_a_long_password/1,
    registration_normalises_the_email/1,
    registration_caps_the_email_at_254_bytes/1,
    registration_needs_one_at_sign/1,
    generated_exports_have_specs/1,
    instructions_name_what_the_app_must_add/1,
    changesets_scrub_the_password/1,
    protected_group_example_uses_neutral_names/1,
    suite_covers_the_limits_and_unauthenticated_requests/1
]).
-export([log/2]).

-define(APP, "blog").
-define(MB, 1048576).

all() ->
    [{group, served}, {group, modules}, {group, sources}].

groups() ->
    [
        {served, [], [
            bearer_scope_does_not_read_an_unauthenticated_body,
            public_scope_refuses_a_large_body_unread,
            public_scope_refuses_a_chunked_body_unread,
            public_scope_accepts_a_small_body,
            login_is_limited_per_email_and_address,
            login_is_limited_per_email_across_addresses,
            malformed_login_spends_no_attempts,
            invalid_utf8_authorization_is_unauthorized
        ]},
        {modules, [], [
            auth_data_has_no_password_hash,
            bearer_token_is_bytewise,
            ipv6_addresses_share_a_bucket_per_64,
            rate_limit_key_uses_nova_auth_trusted_proxies,
            account_limiter_is_created_once,
            limiters_allow_5_per_address_and_50_per_email_in_5_minutes,
            limiters_key_on_a_hash_of_the_email,
            emails_over_254_bytes_are_not_counted,
            token_response_is_not_cached,
            registration_hides_database_errors,
            login_hides_token_errors,
            one_address_cannot_lock_an_account,
            retry_after_rounds_up,
            body_limit_reply_has_no_connection_header,
            bearer_token_allows_only_trailing_padding
        ]},
        {sources, [], [
            files_are_valid_formatted_erlang,
            uses_nova_auth_not_bcrypt_or_thoas,
            drops_the_nova_auth_0_2_workarounds,
            writes_no_migration,
            names_follow_the_app,
            rate_limit_runs_before_the_body_is_decoded,
            protected_group_example_limits_the_body,
            suite_builds_binaries_with_sigils,
            token_schema_has_the_refresh_fields_and_indexes,
            registration_needs_email_and_a_long_password,
            registration_normalises_the_email,
            registration_caps_the_email_at_254_bytes,
            registration_needs_one_at_sign,
            generated_exports_have_specs,
            instructions_name_what_the_app_must_add,
            changesets_scrub_the_password,
            protected_group_example_uses_neutral_names,
            suite_covers_the_limits_and_unauthenticated_requests
        ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(seki),
    _ = compile_source(Config, "nova_auth_password", [
        "-module(nova_auth_password).\n-export([hash/1]).\n",
        "hash(Password) -> <<\"hashed:\", Password/binary>>.\n"
    ]),
    _ = [
        compile_source(Config, Name, rebar3_nova_gen_auth:render(Template, ?APP))
     || {Template, Name} <- [
            {"auth.erl.src", "blog_auth"},
            {"user.erl.src", "blog_user"},
            {"auth_security.erl.src", "blog_auth_security"},
            {"login_limit.erl.src", "blog_login_limit"},
            {"body_limit.erl.src", "blog_body_limit"},
            {"auth_controller.erl.src", "blog_auth_controller"}
        ]
    ],
    compile_source(Config, "blog_accounts", accounts_stub()),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(served, Config) ->
    _ = application:load(nova),
    application:set_env(nova, bootstrap_application, blog),
    application:set_env(nova, environment, dev),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(nova, cowboy_configuration, #{port => 0}),
    application:set_env(nova, plugins, [
        {pre_request, nova_request_plugin, #{decode_json_body => true}}
    ]),
    application:set_env(blog, router_module, rebar3_nova_test_router),
    rebar3_nova_test_router:set_routes(printed_routes()),
    {ok, Started} = application:ensure_all_started(nova),
    [{started, Started} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(served, Config) ->
    {started, Started} = lists:keyfind(started, 1, Config),
    true = is_list(Started),
    _ = [application:stop(App) || App <- lists:reverse(Started), is_atom(App)],
    [
        application:unset_env(nova, Key)
     || Key <- [bootstrap_application, environment, cowboy_configuration, plugins]
    ],
    application:unset_env(blog, router_module),
    rebar3_nova_test_router:set_routes([]),
    %% Empties every limiter, nova_auth_rate_limit's too, so a repeated run
    %% starts with fresh counts. nova_auth caches its limiter as created, so
    %% the limiters stay.
    [
        true = ets:delete_all_objects(limiter_table(Name))
     || {Name, _, _, _, _} <- ets:tab2list(seki_limiter_registry), is_atom(Name)
    ],
    _ = persistent_term:erase(nova_dispatch),
    _ = persistent_term:erase(nova_apps),
    ok;
end_per_group(_Group, _Config) ->
    ok.

%%====================================================================
%% Served with the printed routes
%%====================================================================

%% Pre-request plugins run before security, so an inherited body decoder
%% would read the whole body of a request that has no token at all.
bearer_scope_does_not_read_an_unauthenticated_body(_Config) ->
    {Status, Reads} = traced(fun() ->
        request({127, 0, 1, 1}, "DELETE", "/api/tokens", #{}, binary:copy(~"a", ?MB))
    end),
    ?assertEqual(401, Status),
    ?assertEqual([], Reads).

public_scope_refuses_a_large_body_unread(_Config) ->
    {Status, Reads} = traced(fun() ->
        request({127, 0, 2, 1}, "POST", "/api/users", #{}, binary:copy(~"a", ?MB))
    end),
    ?assertEqual(413, Status),
    ?assertEqual([], Reads).

public_scope_refuses_a_chunked_body_unread(_Config) ->
    Chunk = binary:copy(~"a", 4096),
    Body = [
        [integer_to_list(byte_size(Chunk), 16), "\r\n", Chunk, "\r\n"]
     || _ <- lists:seq(1, 16)
    ],
    {Status, Reads} = traced(fun() ->
        raw({127, 0, 3, 1}, [
            "POST /api/users HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n"
            "content-type: application/json\r\ntransfer-encoding: chunked\r\n\r\n",
            Body,
            "0\r\n\r\n"
        ])
    end),
    ?assertEqual(413, Status),
    ?assertEqual([], Reads).

%% Also shows the tracing in the tests above sees body reads.
public_scope_accepts_a_small_body(_Config) ->
    Body = json:encode(#{email => ~"a@example.com", password => ~"wrong password"}),
    {Status, Reads} = traced(fun() -> request({127, 0, 4, 1}, "POST", "/api/tokens", #{}, Body) end),
    ?assertEqual(401, Status),
    ?assertNotEqual([], Reads).

%% The 6th attempt from one address is refused; another address may still try.
login_is_limited_per_email_and_address(_Config) ->
    Statuses = [login({127, 4, 1, 1}, ~"tiered@example.org") || _ <- lists:seq(1, 6)],
    ?assertEqual(lists:duplicate(5, 401) ++ [429], Statuses),
    ?assertEqual(401, login({127, 4, 2, 1}, ~"tiered@example.org")).

%% 10 addresses with 5 attempts each use up the email's 50, so the next
%% address is refused at once.
login_is_limited_per_email_across_addresses(_Config) ->
    Email = ~"Zoë.Lindqvist@Example.org",
    Statuses = [login({127, 1, N, 1}, Email) || N <- lists:seq(1, 10), _ <- lists:seq(1, 5)],
    ?assertEqual(lists:duplicate(50, 401), Statuses),
    ?assertEqual(429, login({127, 1, 100, 1}, ~"zoë.lindqvist@example.org")),
    Decomposed = unicode:characters_to_nfd_binary(~"ZOË.LINDQVIST@example.org"),
    ?assertNotEqual(~"ZOË.LINDQVIST@example.org", Decomposed),
    ?assertEqual(429, login({127, 1, 101, 1}, Decomposed)),
    ?assertEqual(401, login({127, 1, 99, 1}, ~"someone@example.com")).

%% A body without string email and password is refused before the limits,
%% so it spends none of the email's attempts.
malformed_login_spends_no_attempts(_Config) ->
    Source = {127, 3, 1, 1},
    Malformed = json:encode(#{email => ~"malformed@example.org", password => 1}),
    ?assertEqual(
        lists:duplicate(5, 400),
        [request(Source, "POST", "/api/tokens", #{}, Malformed) || _ <- lists:seq(1, 5)]
    ),
    Statuses = [login(Source, ~"malformed@example.org") || _ <- lists:seq(1, 5)],
    ?assertEqual(lists:duplicate(5, 401), Statuses).

invalid_utf8_authorization_is_unauthorized(_Config) ->
    Headers = #{"authorization" => [255, 255, 255, 255, 255, 255, " abc"]},
    ?assertEqual(401, request({127, 0, 5, 1}, "GET", "/api/me", Headers, ~"")).

%%====================================================================
%% Generated modules called directly
%%====================================================================

auth_data_has_no_password_hash(_Config) ->
    Req = #{headers => #{~"authorization" => ~"Bearer valid-token"}},
    ?assertEqual(
        {true, #{id => 1, email => ~"a@example.com"}}, blog_auth_security:require_bearer(Req)
    ).

bearer_token_is_bytewise(_Config) ->
    Token = fun(Header) ->
        blog_auth_security:bearer_token(#{headers => #{~"authorization" => Header}})
    end,
    ?assertEqual({ok, ~"abc-._~+/="}, Token(~"Bearer abc-._~+/=")),
    ?assertEqual({ok, ~"abc"}, Token(~"bEaReR abc")),
    ?assertEqual(error, Token(~"Bearer a b")),
    ?assertEqual(error, Token(~"Bearer  abc")),
    ?assertEqual(error, Token(~"Bearer ")),
    ?assertEqual(error, Token(<<"Bearer ", 255, 254>>)),
    ?assertEqual(error, Token(<<255, 255, 255, 255, 255, 255, " abc">>)),
    ?assertEqual(error, Token(~"Basic Zm9vOmJhcg==")),
    ?assertMatch(
        {false, 401, _, _},
        blog_auth_security:require_bearer(#{headers => #{~"authorization" => ~"Bearer a b"}})
    ).

ipv6_addresses_share_a_bucket_per_64(_Config) ->
    Key = fun(IP) -> blog_login_limit:key(req(IP, ~"")) end,
    ?assertEqual(
        Key({16#2001, 16#db8, 1, 2, 0, 0, 0, 1}), Key({16#2001, 16#db8, 1, 2, 9, 9, 9, 9})
    ),
    ?assertNotEqual(
        Key({16#2001, 16#db8, 1, 2, 0, 0, 0, 1}), Key({16#2001, 16#db8, 1, 3, 0, 0, 0, 1})
    ),
    ?assertNotEqual(Key({192, 0, 2, 1}), Key({192, 0, 2, 2})).

%% The address comes from nova_auth_rate_limit:client_ip/1, so behind a
%% proxy the key follows nova_auth's trusted_proxies.
rate_limit_key_uses_nova_auth_trusted_proxies(_Config) ->
    Req = req({10, 0, 0, 2}, ~"203.0.113.7"),
    ?assertEqual({10, 0, 0, 2}, blog_login_limit:key(Req)),
    application:set_env(nova_auth, trusted_proxies, [{{10, 0, 0, 0}, 8}]),
    try
        ?assertEqual({203, 0, 113, 7}, blog_login_limit:key(Req)),
        ?assertEqual(
            {16#2001, 16#db8, 1, 2, 0, 0, 0, 0},
            blog_login_limit:key(req({10, 0, 0, 2}, ~"2001:db8:1:2::9"))
        )
    after
        application:unset_env(nova_auth, trusted_proxies)
    end.

%% seki logs a warning on every new_limiter call for a name it already has.
account_limiter_is_created_once(_Config) ->
    ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{pid => self()}}),
    try
        Emails = [
            iolist_to_binary([~"once-", integer_to_list(N), ~"@example.com"])
         || N <- lists:seq(1, 5)
        ],
        Checks = [blog_login_limit:check(E, req({192, 0, 2, 1}, ~"")) || E <- Emails],
        ?assertEqual(lists:duplicate(5, allow), Checks),
        ?assertEqual([], seki_warnings())
    after
        logger:remove_handler(?MODULE)
    end.

%% seki's cleanup drops rows older than 10 minutes, so a longer window
%% would reset early.
limiters_allow_5_per_address_and_50_per_email_in_5_minutes(_Config) ->
    allow = blog_login_limit:check(~"windows@example.org", req({192, 0, 2, 2}, ~"")),
    ?assertEqual(
        [#{limit => 5, window => 300000}, #{limit => 50, window => 300000}],
        [limiter_config(Name) || Name <- limiters()]
    ).

%% The tables hold a SHA-256 of the normalised email, never the email.
limiters_key_on_a_hash_of_the_email(_Config) ->
    allow = blog_login_limit:check(~"Hashed.Key@Example.org", req({192, 0, 2, 3}, ~"")),
    Rows = term_to_binary(limiter_rows()),
    ?assertNotEqual(nomatch, binary:match(Rows, crypto:hash(sha256, ~"hashed.key@example.org"))),
    ?assertEqual(nomatch, binary:match(Rows, [~"hashed.key", ~"Hashed.Key"])).

%% No account has an email over 254 bytes, so such an email takes no room in
%% the tables, however long it is.
emails_over_254_bytes_are_not_counted(_Config) ->
    Req = req({192, 0, 2, 4}, ~""),
    allow = blog_login_limit:check(~"first@example.org", Req),
    Before = limiter_rows(),
    ?assertEqual(allow, blog_login_limit:check(email_of(255), Req)),
    ?assertEqual(allow, blog_login_limit:check(email_of(1000000), Req)),
    ?assertEqual(Before, limiter_rows()),
    ?assertEqual(allow, blog_login_limit:check(email_of(254), Req)),
    ?assertNotEqual(Before, limiter_rows()).

%% RFC 6749 section 5.1.
token_response_is_not_cached(_Config) ->
    Json = #{~"email" => ~"reader@example.org", ~"password" => ~"the right password"},
    {json, 201, Headers, #{~"token" := ~"issued-token"}} =
        blog_auth_controller:login((req({192, 0, 2, 5}, ~""))#{json => Json}),
    ?assertEqual(#{~"cache-control" => ~"no-store", ~"pragma" => ~"no-cache"}, Headers).

%% A base error is the database failing; its text goes to the log, not the client.
registration_hides_database_errors(_Config) ->
    ?assertEqual(
        {json, 503, #{}, #{~"error" => ~"try again later"}},
        blog_auth_controller:register(#{json => #{~"email" => ~"down@example.org"}})
    ),
    ?assertEqual(
        {json, 422, #{}, #{~"errors" => #{~"email" => [~"has already been taken"]}}},
        blog_auth_controller:register(#{json => #{~"email" => ~"taken@example.org"}})
    ).

login_hides_token_errors(_Config) ->
    Json = #{~"email" => ~"tokenless@example.org", ~"password" => ~"the right password"},
    ?assertEqual(
        {json, 503, #{}, #{~"error" => ~"try again later"}},
        blog_auth_controller:login((req({192, 0, 2, 6}, ~""))#{json => Json})
    ).

%% Past its own 5 attempts an address spends none of the email's 50, so it
%% cannot lock the account by itself.
one_address_cannot_lock_an_account(_Config) ->
    Email = ~"one-address@example.org",
    Checks = [blog_login_limit:check(Email, req({192, 0, 2, 7}, ~"")) || _ <- lists:seq(1, 50)],
    ?assertEqual(lists:duplicate(5, allow), lists:sublist(Checks, 5)),
    ?assertEqual([], [Check || Check <- lists:nthtail(5, Checks), Check =:= allow]),
    ?assertEqual(allow, blog_login_limit:check(Email, req({192, 0, 2, 8}, ~""))).

%% seki answers 60001 ms here. Rounded down that would be 60 s, a second
%% too soon; rounded up as nova_auth_rate_limit does, it is 61.
retry_after_rounds_up(_Config) ->
    Req = req({192, 0, 2, 9}, ~""),
    Email = ~"retry@example.org",
    [allow = blog_login_limit:check(Email, Req) || _ <- lists:seq(1, 5)],
    ?assertEqual({deny, 61}, blog_login_limit:check(Email, Req)).

body_limit_reply_has_no_connection_header(_Config) ->
    Req = #{pid => self(), streamid => 1, method => ~"POST", body_length => ?MB},
    {stop, _, state} = blog_body_limit:pre_request(Req, #{}, #{}, state),
    receive
        {{_, 1}, {response, 413, Headers, _Body}} ->
            ?assertEqual(false, maps:is_key(~"connection", Headers))
    after 1000 -> ct:fail(no_response)
    end.

bearer_token_allows_only_trailing_padding(_Config) ->
    Token = fun(Header) ->
        blog_auth_security:bearer_token(#{headers => #{~"authorization" => Header}})
    end,
    ?assertEqual({ok, ~"abc=="}, Token(~"Bearer abc==")),
    ?assertEqual(error, Token(~"Bearer ab=c")),
    ?assertEqual(error, Token(~"Bearer =abc")),
    ?assertEqual(error, Token(~"Bearer ==")).

%%====================================================================
%% Generated sources
%%====================================================================

files_are_valid_formatted_erlang(_Config) ->
    [assert_formatted(rebar3_nova_gen_auth:render(T, ?APP)) || {T, _} <- auth_files()].

uses_nova_auth_not_bcrypt_or_thoas(_Config) ->
    Sources = [rebar3_nova_gen_auth:render(T, ?APP) || {T, _} <- auth_files()],
    [?assertEqual(nomatch, binary:match(S, [~"bcrypt", ~"thoas", ~"citext"])) || S <- Sources],
    All = iolist_to_binary(Sources),
    ?assertNotEqual(nomatch, binary:match(All, ~"nova_auth_password:hash")),
    ?assertNotEqual(nomatch, binary:match(All, ~"nova_auth_accounts:register(blog_auth, Params)")),
    ?assertNotEqual(nomatch, binary:match(All, ~"nova_auth_accounts:authenticate")),
    ?assertNotEqual(nomatch, binary:match(All, ~"nova_auth_rate_limit:client_ip(Req)")),
    ?assertNotEqual(nomatch, binary:match(All, ~"-behaviour(nova_auth).")).

%% nova_auth 0.3 fixes the rate-limit plugin and has client_ip/1, so the
%% wrapper plugin and the copied address handling are gone.
drops_the_nova_auth_0_2_workarounds(_Config) ->
    Paths = [P || {_, P} <- auth_files()],
    ?assertEqual([], [P || P <- Paths, string:find(P, "rate_limit") =/= nomatch]),
    Sources = iolist_to_binary([
        rebar3_nova_gen_auth:render(T, ?APP)
     || {T, P} <- auth_files(), string:find(P, "_SUITE") =:= nomatch
    ]),
    ?assertEqual(
        nomatch, binary:match(Sources, [~"key_fun", ~"x-forwarded-for", ~"blog_rate_limit"])
    ),
    Text = rebar3_nova_gen_auth:instructions(?APP),
    ?assertEqual(nomatch, string:find(Text, "key_fun")),
    ?assertEqual(nomatch, string:find(Text, "blog_rate_limit")).

writes_no_migration(_Config) ->
    ?assertEqual([], [P || {_, P} <- auth_files(), string:find(P, "migration") =/= nomatch]).

names_follow_the_app(_Config) ->
    Paths = [P || {_, P} <- auth_files()],
    ?assert(lists:member("src/blog_auth.erl", Paths)),
    ?assert(lists:member("src/blog_login_limit.erl", Paths)),
    ?assert(lists:member("src/schemas/blog_user.erl", Paths)),
    ?assert(lists:member("src/controllers/blog_auth_controller.erl", Paths)),
    [
        ?assertEqual(nomatch, binary:match(rebar3_nova_gen_auth:render(T, ?APP), ~"{{app}}"))
     || {T, _} <- auth_files()
    ].

rate_limit_runs_before_the_body_is_decoded(_Config) ->
    Instructions = rebar3_nova_gen_auth:instructions(?APP),
    Start = string:str(Instructions, "4. Add the routes"),
    Text = string:substr(Instructions, Start, string:str(Instructions, "\n\n5.") - Start),
    Limiter = string:str(
        Text, "{pre_request, nova_auth_rate_limit, #{key => {blog_login_limit, key}}}"
    ),
    BodyLimit = string:str(Text, "{pre_request, blog_body_limit, #{max_bytes => 65536}}"),
    Decoder = string:str(Text, "{pre_request, nova_request_plugin"),
    ?assert(Limiter > 0),
    ?assert(Limiter < BodyLimit),
    ?assert(BodyLimit < Decoder),
    ?assertEqual(1, length(string:split(Text, "{pre_request, nova_request_plugin", all)) - 1),
    ?assertNotEqual(nomatch, string:find(Text, "plugins => [],")).

protected_group_example_limits_the_body(_Config) ->
    Text = rebar3_nova_gen_auth:instructions(?APP),
    Step = step(Text, 5),
    Group = eval_term(string:substr(Step, string:str(Step, "#{prefix"))),
    ?assertEqual(fun blog_auth_security:require_bearer/1, maps:get(security, Group)),
    ?assertEqual(
        [
            {pre_request, blog_body_limit, #{}},
            {pre_request, nova_request_plugin, #{decode_json_body => true}}
        ],
        maps:get(plugins, Group)
    ).

suite_builds_binaries_with_sigils(_Config) ->
    ?assertEqual(nomatch, binary:match(render_auth("auth_SUITE.erl.src"), ~"<<\"")).

token_schema_has_the_refresh_fields_and_indexes(Config) ->
    Token = compile_source(Config, "blog_user_token", render_auth("user_token.erl.src")),
    Types = #{F#kura_field.name => F#kura_field.type || F <- Token:fields()},
    ?assertMatch(#{family_id := string, used_at := utc_datetime}, Types),
    ?assertEqual(
        [[context, token], [user_id], [family_id]], [Fields || {Fields, _} <- Token:indexes()]
    ).

registration_needs_email_and_a_long_password(_Config) ->
    Valid = registration(~"reader@example.org", ~"twelve chars"),
    ?assert(Valid#kura_changeset.valid),
    ?assertEqual(~"hashed:twelve chars", kura_changeset:get_change(Valid, hashed_password)),
    Short = registration(~"reader@example.org", ~"eleven char"),
    ?assertMatch([{password, _}], Short#kura_changeset.errors),
    ?assertEqual(undefined, kura_changeset:get_change(Short, hashed_password)),
    Missing = blog_user:registration_changeset(#{}, #{~"password" => ~"twelve chars"}),
    ?assertMatch([{email, _}], Missing#kura_changeset.errors).

registration_normalises_the_email(_Config) ->
    Changeset = registration(unicode:characters_to_nfd_binary(~"Zoë@Example.ORG"), ~"twelve chars"),
    ?assertEqual(~"zoë@example.org", kura_changeset:get_change(Changeset, email)).

%% Counted in bytes, as the login limit counts them.
registration_caps_the_email_at_254_bytes(_Config) ->
    ?assert((registration(email_of(254), ~"twelve chars"))#kura_changeset.valid),
    TooLong = registration(email_of(255), ~"twelve chars"),
    ?assertMatch([{email, _}], TooLong#kura_changeset.errors),
    ?assertEqual(undefined, kura_changeset:get_change(TooLong, hashed_password)),
    Wide = registration(<<(binary:copy(~"ö", 122))/binary, "@example.org">>, ~"twelve chars"),
    ?assertMatch([{email, _}], Wide#kura_changeset.errors).

registration_needs_one_at_sign(_Config) ->
    [
        ?assertMatch(
            [{email, _}], (registration(Email, ~"twelve chars"))#kura_changeset.errors, Email
        )
     || Email <- [
            ~"reader.example.org",
            ~"reader@@example.org",
            ~"a@b@example.org",
            ~"@example.org",
            ~"reader@"
        ]
    ],
    ?assert((registration(~"a@b", ~"twelve chars"))#kura_changeset.valid).

%% Every exported function of the generated modules has a -spec.
generated_exports_have_specs(Config) ->
    Unspecced = [
        {Template, unspecced(Config, Template)}
     || {Template, Path} <- auth_files(), string:find(Path, "_SUITE") =:= nomatch
    ],
    ?assertEqual([], [U || {_, [_ | _]} = U <- Unspecced]).

%% The prose lives in the README; these are what the printed steps must name.
instructions_name_what_the_app_must_add(_Config) ->
    Text = rebar3_nova_gen_auth:instructions(?APP),
    [
        ?assertNotEqual(nomatch, string:find(Text, Needle), Needle)
     || Needle <- [
            "{nova_auth, \"~> 0.3\"}",
            "{seki, \"~> 0.4\"}",
            "kura_migrator:migrate(blog_repo) as a release step, not in blog_app:start/2",
            "Only if the app has no session or cookie-based pages",
            "{blog, [{use_sessions, false}]}",
            "{nova_auth, [{trusted_proxies, [",
            "See README: gen_auth caveats."
        ]
    ],
    ?assertEqual(nomatch, string:find(Text, "~> 0.2")).

%% A changeset comes back in {error, CS} and can reach a log or crash report.
changesets_scrub_the_password(_Config) ->
    assert_no_plaintext(registration(~"reader@example.org", ~"twelve chars")),
    assert_no_plaintext(registration(~"reader@example.org", ~"short")),
    Change = blog_user:password_changeset(#{id => 1}, #{~"password" => ~"a new passphrase"}),
    ?assert(Change#kura_changeset.valid),
    ?assertEqual(~"hashed:a new passphrase", kura_changeset:get_change(Change, hashed_password)),
    assert_no_plaintext(Change).

protected_group_example_uses_neutral_names(_Config) ->
    Step = step(rebar3_nova_gen_auth:instructions(?APP), 5),
    Group = eval_term(string:substr(Step, string:str(Step, "#{prefix"))),
    ?assertMatch(
        [{"/items", _, #{methods := [post]}}], maps:get(routes, Group)
    ),
    [{_, Create, _}] = maps:get(routes, Group),
    ?assertEqual(fun blog_item_controller:create/1, Create),
    ?assertEqual(nomatch, string:find(Step, "post_controller")).

%% The generated suite runs against a database, so its cases are checked
%% here and run in an app that has one.
suite_covers_the_limits_and_unauthenticated_requests(Config) ->
    Source = render_auth("auth_SUITE.erl.src"),
    Suite = compile_source(Config, "blog_auth_SUITE", Source),
    Cases = Suite:all(),
    [
        ?assert(lists:member(Case, Cases), Case)
     || Case <- [
            unknown_email_is_rejected_like_a_wrong_password,
            me_needs_an_authorization_header,
            large_body_is_refused,
            logins_from_one_address_are_limited,
            logins_for_one_email_are_limited,
            bearer_routes_do_not_read_an_unauthenticated_body
        ]
    ],
    ?assertNotEqual(nomatch, binary:match(Source, ~"{ok, _} = kura_migrator:migrate(blog_repo)")),
    %% Each test is a fresh client in its own /64, the unit the limits count.
    Clients = [
        proplists:get_value(client, Suite:init_per_testcase(register_login_and_logout, []))
     || _ <- lists:seq(1, 20)
    ],
    [?assertMatch("2001:db8:" ++ _, Client) || Client <- Clients],
    ?assertEqual(20, length(lists:usort(Clients))).

%%====================================================================
%% Helpers
%%====================================================================

%% Step N of the printed instructions, up to the next step or the
%% closing pointer to the README.
step(Text, N) ->
    Start = string:str(Text, integer_to_list(N) ++ ". "),
    Rest = string:substr(Text, Start),
    case string:str(Rest, "\n\n" ++ integer_to_list(N + 1) ++ ". ") of
        0 -> string:substr(Rest, 1, string:str(Rest, "\n\nSee"));
        End -> string:substr(Rest, 1, End)
    end.

auth_files() ->
    rebar3_nova_gen_auth:files(?APP).

render_auth(Template) ->
    rebar3_nova_gen_auth:render(Template, ?APP).

registration(Email, Password) ->
    blog_user:registration_changeset(#{}, #{~"email" => Email, ~"password" => Password}).

assert_no_plaintext(#kura_changeset{changes = Changes, params = Params}) ->
    ?assertEqual([], [K || K <- [password, ~"password"], maps:is_key(K, Changes)]),
    ?assertEqual([], [K || K <- [password, ~"password"], maps:is_key(K, Params)]).

assert_formatted(Source) ->
    Text = binary_to_list(Source),
    {ok, Formatted, _Warnings} = erlfmt:format_string(Text, []),
    ?assertEqual(Text, Formatted).

-spec eval_term(string()) -> dynamic().
eval_term(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression ++ "."),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:expr(Expr, []),
    Value.

log(#{meta := #{domain := [seki | _]}} = Event, #{config := #{pid := Pid}}) ->
    Pid ! {seki_log, Event};
log(_Event, _Config) ->
    ok.

seki_warnings() ->
    receive
        {seki_log, Event} -> [Event | seki_warnings()]
    after 100 -> []
    end.

%% The route groups from step 4 of the printed instructions.
printed_routes() ->
    Text = rebar3_nova_gen_auth:instructions(?APP),
    Start = string:str(Text, "     #{prefix"),
    Groups = string:substr(Text, Start, string:str(Text, "\n\n5.") - Start),
    {ok, Tokens, _} = erl_scan:string("[" ++ Groups ++ "]."),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    {value, Routes, _} = erl_eval:expr(Expr, []),
    Routes.

login(Source, Email) ->
    request(
        Source, "POST", "/api/tokens", #{}, json:encode(#{email => Email, password => ~"guess"})
    ).

%% An email of Bytes bytes.
email_of(Bytes) ->
    <<(binary:copy(~"a", Bytes - 12))/binary, "@example.org">>.

limiters() ->
    [blog_login_limit_client, blog_login_limit_account].

limiter_config(Name) ->
    {_Algorithm, _Backend, _Tab, Config} = seki_limiter_registry:lookup(Name),
    Config.

limiter_rows() ->
    [lists:sort(ets:tab2list(limiter_table(Name))) || Name <- limiters()].

%% seki types a backend's state as term(); seki_backend_ets keeps its table there.
-spec limiter_table(atom()) -> dynamic().
limiter_table(Name) ->
    {_Algorithm, seki_backend_ets, Tab, _Config} = seki_limiter_registry:lookup(Name),
    Tab.

unspecced(Config, Template) ->
    File = ?config(priv_dir, Config) ++ "/specs_" ++ Template,
    ok = file:write_file(File, render_auth(Template)),
    {ok, Forms} = epp:parse_file(File, []),
    Exports = [F || {attribute, _, export, Functions} <- Forms, is_list(Functions), F <- Functions],
    Exports -- [Function || {attribute, _, spec, {Function, _}} <- Forms].

%% The port Nova bound for the served group, which asks for any free one.
port() ->
    [Port] = [Port || _ := #{port := Port} <- ranch:info()],
    Port.

accounts_stub() ->
    ~"""
    -module(blog_accounts).
    -include_lib("kura/include/kura.hrl").
    -export([authenticate/2, issue_token/1, user_for_token/1, register/1]).

    authenticate(~"reader@example.org", ~"the right password") ->
        {ok, #{id => 2, email => ~"reader@example.org"}};
    authenticate(~"tokenless@example.org", ~"the right password") ->
        {ok, #{id => 3, email => ~"tokenless@example.org"}};
    authenticate(_Email, _Password) ->
        {error, invalid_credentials}.

    issue_token(#{id := 2}) -> {ok, ~"issued-token"};
    issue_token(#{id := 3}) -> {error, connection_closed}.

    user_for_token(~"valid-token") ->
        {ok, #{
            id => 1,
            email => ~"a@example.com",
            hashed_password => ~"$pbkdf2-sha256$600000$c2FsdA$aGFzaA",
            confirmed_at => undefined
        }};
    user_for_token(_Token) ->
        {error, not_found}.

    register(#{~"email" := ~"down@example.org"}) ->
        {error, #kura_changeset{errors = [{base, ~"connection refused by 10.0.0.5"}]}};
    register(#{~"email" := ~"taken@example.org"}) ->
        {error, #kura_changeset{errors = [{email, ~"has already been taken"}]}}.
    """.

req(Peer, ForwardedFor) ->
    #{peer => {Peer, 443}, headers => #{~"x-forwarded-for" => ForwardedFor}}.

%% Calls to cowboy_req:read_body/1,2 made while Fun runs.
traced(Fun) ->
    {module, cowboy_req} = code:ensure_loaded(cowboy_req),
    2 = erlang:trace_pattern({cowboy_req, read_body, '_'}, true, [global]),
    erlang:trace(new_processes, true, [call, {tracer, self()}]),
    try
        Result = Fun(),
        {Result, reads()}
    after
        erlang:trace(new_processes, false, [call]),
        erlang:trace_pattern({cowboy_req, read_body, '_'}, false, [global])
    end.

reads() ->
    receive
        {trace, _Pid, call, {cowboy_req, read_body, _Args}} = Read -> [Read | reads()]
    after 200 -> []
    end.

request(Source, Method, Path, Headers, Body) ->
    HeaderLines = [[Name, ": ", Value, "\r\n"] || Name := Value <- Headers],
    raw(Source, [
        Method,
        " ",
        Path,
        " HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n",
        "content-type: application/json\r\ncontent-length: ",
        integer_to_list(iolist_size(Body)),
        "\r\n",
        HeaderLines,
        "\r\n",
        Body
    ]).

%% Sends a request from Source and returns the response status. The server
%% may answer and close before the whole body is sent.
raw(Source, Request) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, port(), [binary, {active, false}, {ip, Source}]),
    _ = gen_tcp:send(Socket, Request),
    {ok, Response} = gen_tcp:recv(Socket, 0, 10000),
    ok = gen_tcp:close(Socket),
    <<"HTTP/1.1 ", Status:3/binary, _/binary>> = Response,
    binary_to_integer(Status).

compile_source(Config, Name, Source) ->
    File = ?config(priv_dir, Config) ++ "/" ++ Name ++ ".erl",
    ok = file:write_file(File, Source),
    {ok, Module, Binary} = compile:file(File, [binary, return_errors]),
    true = is_atom(Module) andalso is_binary(Binary),
    {module, Module} = code:load_binary(Module, File, Binary),
    Module.
