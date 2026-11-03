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
    login_is_limited_per_email_across_addresses/1,
    invalid_utf8_authorization_is_unauthorized/1,
    auth_data_has_no_password_hash/1,
    bearer_token_is_bytewise/1,
    ipv6_addresses_share_a_bucket_per_64/1,
    rate_limit_key_uses_nova_auth_trusted_proxies/1,
    account_limiter_is_created_once/1,
    body_limit_reply_has_no_connection_header/1,
    bearer_token_allows_only_trailing_padding/1,
    files_are_valid_formatted_erlang/1,
    uses_nova_auth_not_bcrypt_or_thoas/1,
    drops_the_nova_auth_0_2_workarounds/1,
    writes_no_migration/1,
    names_follow_the_app/1,
    instructions_target_nova_auth_0_3/1,
    instructions_run_migrations_as_a_release_step/1,
    rate_limit_runs_before_the_body_is_decoded/1,
    instructions_cover_sessions_enumeration_and_proxies/1,
    protected_group_example_limits_the_body/1,
    suite_builds_binaries_with_sigils/1,
    token_schema_has_the_refresh_fields_and_indexes/1,
    registration_needs_email_and_a_long_password/1,
    registration_normalises_the_email/1,
    changesets_scrub_the_password/1
]).
-export([log/2]).

-define(APP, "blog").
-define(PORT, 58632).
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
            login_is_limited_per_email_across_addresses,
            invalid_utf8_authorization_is_unauthorized
        ]},
        {modules, [], [
            auth_data_has_no_password_hash,
            bearer_token_is_bytewise,
            ipv6_addresses_share_a_bucket_per_64,
            rate_limit_key_uses_nova_auth_trusted_proxies,
            account_limiter_is_created_once,
            body_limit_reply_has_no_connection_header,
            bearer_token_allows_only_trailing_padding
        ]},
        {sources, [], [
            files_are_valid_formatted_erlang,
            uses_nova_auth_not_bcrypt_or_thoas,
            drops_the_nova_auth_0_2_workarounds,
            writes_no_migration,
            names_follow_the_app,
            instructions_target_nova_auth_0_3,
            instructions_run_migrations_as_a_release_step,
            rate_limit_runs_before_the_body_is_decoded,
            instructions_cover_sessions_enumeration_and_proxies,
            protected_group_example_limits_the_body,
            suite_builds_binaries_with_sigils,
            token_schema_has_the_refresh_fields_and_indexes,
            registration_needs_email_and_a_long_password,
            registration_normalises_the_email,
            changesets_scrub_the_password
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
    application:set_env(nova, cowboy_configuration, #{port => ?PORT}),
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
    ok = seki:delete_limiter(blog_login_limit),
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

%% Each attempt comes from a fresh address, so only the per-email limit
%% can stop the 21st.
login_is_limited_per_email_across_addresses(_Config) ->
    Login = fun(Source, Email) ->
        Body = json:encode(#{email => Email, password => ~"guess"}),
        request(Source, "POST", "/api/tokens", #{}, Body)
    end,
    Statuses = [Login({127, 1, N, 1}, ~"Zoë.Lindqvist@Example.org") || N <- lists:seq(1, 21)],
    ?assertEqual(lists:duplicate(20, 401) ++ [429], Statuses),
    ?assertEqual(429, Login({127, 1, 100, 1}, ~"zoë.lindqvist@example.org")),
    Decomposed = unicode:characters_to_nfd_binary(~"ZOË.LINDQVIST@example.org"),
    ?assertNotEqual(~"ZOË.LINDQVIST@example.org", Decomposed),
    ?assertEqual(429, Login({127, 1, 101, 1}, Decomposed)),
    Other = json:encode(#{email => ~"someone@example.com", password => ~"guess"}),
    ?assertEqual(401, request({127, 1, 99, 1}, "POST", "/api/tokens", #{}, Other)).

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
        ?assertEqual(lists:duplicate(5, allow), [blog_login_limit:check(E) || E <- Emails]),
        ?assertEqual([], seki_warnings())
    after
        logger:remove_handler(?MODULE)
    end.

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
    Sources = iolist_to_binary([rebar3_nova_gen_auth:render(T, ?APP) || {T, _} <- auth_files()]),
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

instructions_target_nova_auth_0_3(_Config) ->
    Text = rebar3_nova_gen_auth:instructions(?APP),
    ?assertNotEqual(nomatch, string:find(Text, "{nova_auth, \"~> 0.3\"}")),
    ?assertEqual(nomatch, string:find(Text, "~> 0.2")).

instructions_run_migrations_as_a_release_step(_Config) ->
    Text = rebar3_nova_gen_auth:instructions(?APP),
    ?assertNotEqual(
        nomatch, string:find(Text, "kura_migrator:migrate(blog_repo) as a release step")
    ),
    ?assertNotEqual(nomatch, string:find(Text, "not in blog_app:start/2")).

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
    ?assertEqual(1, length(string:split(Text, "nova_request_plugin", all)) - 1),
    ?assertNotEqual(nomatch, string:find(Text, "plugins => [],")).

instructions_cover_sessions_enumeration_and_proxies(_Config) ->
    Text = rebar3_nova_gen_auth:instructions(?APP),
    [
        ?assertNotEqual(nomatch, string:find(Text, Needle), Needle)
     || Needle <- [
            "{blog, [{use_sessions, false}]}",
            "tells anyone which emails have accounts",
            "{nova_auth, [{trusted_proxies, [",
            "20 attempts per email"
        ]
    ].

protected_group_example_limits_the_body(_Config) ->
    Text = rebar3_nova_gen_auth:instructions(?APP),
    Start = string:str(Text, "5. Protect"),
    Step = string:substr(Text, Start, string:str(Text, "\n\n6.") - Start),
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

%% A changeset comes back in {error, CS} and can reach a log or crash report.
changesets_scrub_the_password(_Config) ->
    assert_no_plaintext(registration(~"reader@example.org", ~"twelve chars")),
    assert_no_plaintext(registration(~"reader@example.org", ~"short")),
    Change = blog_user:password_changeset(#{id => 1}, #{~"password" => ~"a new passphrase"}),
    ?assert(Change#kura_changeset.valid),
    ?assertEqual(~"hashed:a new passphrase", kura_changeset:get_change(Change, hashed_password)),
    assert_no_plaintext(Change).

%%====================================================================
%% Helpers
%%====================================================================

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

accounts_stub() ->
    ~"""
    -module(blog_accounts).
    -export([authenticate/2, user_for_token/1, register/1]).

    authenticate(_Email, _Password) -> {error, invalid_credentials}.

    user_for_token(~"valid-token") ->
        {ok, #{
            id => 1,
            email => ~"a@example.com",
            hashed_password => ~"$pbkdf2-sha256$600000$c2FsdA$aGFzaA",
            confirmed_at => undefined
        }};
    user_for_token(_Token) ->
        {error, not_found}.

    register(_Params) -> {error, not_used}.
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
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?PORT, [binary, {active, false}, {ip, Source}]),
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
