-module(rebar3_nova_gen_auth_SUITE).

%% Runs the gen_auth output: the generated plugins, security and controller
%% modules are compiled and served by Nova with the routes the instructions
%% print. Only the accounts module is a stub, so no database is needed.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

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
    peer_is_the_client_without_trusted_proxies/1,
    trusted_proxy_resolves_the_rightmost_untrusted_hop/1,
    trusted_proxies_take_cidr_ranges/1,
    mapped_addresses_are_unmapped/1,
    ipv6_addresses_share_a_bucket_per_64/1,
    forwarded_hops_may_carry_a_port/1,
    a_mistyped_trusted_proxy_fails_loudly/1,
    account_limiter_is_created_once/1,
    body_limit_reply_has_no_connection_header/1,
    bearer_token_allows_only_trailing_padding/1
]).
-export([log/2]).

-define(APP, "blog").
-define(PORT, 58632).
-define(MB, 1048576).

all() ->
    [{group, served}, {group, modules}].

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
            peer_is_the_client_without_trusted_proxies,
            trusted_proxy_resolves_the_rightmost_untrusted_hop,
            trusted_proxies_take_cidr_ranges,
            mapped_addresses_are_unmapped,
            ipv6_addresses_share_a_bucket_per_64,
            forwarded_hops_may_carry_a_port,
            a_mistyped_trusted_proxy_fails_loudly,
            account_limiter_is_created_once,
            body_limit_reply_has_no_connection_header,
            bearer_token_allows_only_trailing_padding
        ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(seki),
    _ = [
        compile_source(Config, Name, rebar3_nova_gen_auth:render(Template, ?APP))
     || {Template, Name} <- [
            {"user.erl.src", "blog_user"},
            {"auth_security.erl.src", "blog_auth_security"},
            {"rate_limit.erl.src", "blog_rate_limit"},
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
    ok = seki:delete_limiter(blog_account_rate_limit),
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
    Statuses = [Login({127, 1, N, 1}, ~"Vïctim@Example.com") || N <- lists:seq(1, 21)],
    ?assertEqual(lists:duplicate(20, 401) ++ [429], Statuses),
    ?assertEqual(429, Login({127, 1, 100, 1}, ~"vïctim@example.com")),
    Decomposed = unicode:characters_to_nfd_binary(~"VÏCTIM@example.com"),
    ?assertNotEqual(~"VÏCTIM@example.com", Decomposed),
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

peer_is_the_client_without_trusted_proxies(_Config) ->
    application:unset_env(blog, trusted_proxies),
    ?assertEqual({10, 0, 0, 2}, blog_rate_limit:client_ip(req({10, 0, 0, 2}, ~"203.0.113.7"))).

trusted_proxy_resolves_the_rightmost_untrusted_hop(_Config) ->
    with_trusted([{10, 0, 0, 2}, {10, 0, 0, 3}], fun() ->
        Proxy = {10, 0, 0, 2},
        Client = {203, 0, 113, 7},
        ?assertEqual(Client, blog_rate_limit:client_ip(req(Client, ~"198.51.100.1"))),
        ?assertEqual(Client, blog_rate_limit:client_ip(req(Proxy, ~"198.51.100.1, 203.0.113.7"))),
        ?assertEqual(Client, blog_rate_limit:client_ip(req(Proxy, ~"203.0.113.7, 10.0.0.3"))),
        ?assertEqual(Proxy, blog_rate_limit:client_ip(req(Proxy, ~"10.0.0.3"))),
        ?assertEqual(Proxy, blog_rate_limit:client_ip(req(Proxy, ~"203.0.113.7, not-an-ip"))),
        ?assertEqual(Proxy, blog_rate_limit:client_ip(req(Proxy, <<255, 254>>))),
        ?assertEqual(Proxy, blog_rate_limit:client_ip(#{peer => {Proxy, 443}, headers => #{}}))
    end).

trusted_proxies_take_cidr_ranges(_Config) ->
    with_trusted([{{10, 1, 0, 0}, 16}, {{16#fd00, 0, 0, 0, 0, 0, 0, 0}, 8}], fun() ->
        ?assertEqual(
            {203, 0, 113, 7},
            blog_rate_limit:client_ip(req({10, 1, 200, 3}, ~"203.0.113.7, 10.1.0.9"))
        ),
        ?assertEqual(
            {203, 0, 113, 7},
            blog_rate_limit:client_ip(req({16#fd12, 0, 0, 0, 0, 0, 0, 1}, ~"203.0.113.7"))
        ),
        ?assertEqual({10, 2, 0, 1}, blog_rate_limit:client_ip(req({10, 2, 0, 1}, ~"203.0.113.7")))
    end).

mapped_addresses_are_unmapped(_Config) ->
    Mapped = {0, 0, 0, 0, 0, 16#ffff, 16#cb00, 16#7107},
    application:unset_env(blog, trusted_proxies),
    ?assertEqual({203, 0, 113, 7}, blog_rate_limit:client_ip(req(Mapped, ~""))),
    ?assertEqual(
        blog_rate_limit:rate_limit_key(req({203, 0, 113, 7}, ~"")),
        blog_rate_limit:rate_limit_key(req(Mapped, ~""))
    ),
    with_trusted([{10, 0, 0, 2}], fun() ->
        MappedProxy = {0, 0, 0, 0, 0, 16#ffff, 16#0a00, 16#0002},
        ?assertEqual(
            {203, 0, 113, 7},
            blog_rate_limit:client_ip(req(MappedProxy, ~"::ffff:203.0.113.7"))
        )
    end).

ipv6_addresses_share_a_bucket_per_64(_Config) ->
    application:unset_env(blog, trusted_proxies),
    Key = fun(IP) -> blog_rate_limit:rate_limit_key(req(IP, ~"")) end,
    ?assertEqual(
        Key({16#2001, 16#db8, 1, 2, 0, 0, 0, 1}), Key({16#2001, 16#db8, 1, 2, 9, 9, 9, 9})
    ),
    ?assertNotEqual(
        Key({16#2001, 16#db8, 1, 2, 0, 0, 0, 1}), Key({16#2001, 16#db8, 1, 3, 0, 0, 0, 1})
    ),
    ?assertNotEqual(Key({192, 0, 2, 1}), Key({192, 0, 2, 2})).

forwarded_hops_may_carry_a_port(_Config) ->
    with_trusted([{10, 0, 0, 2}], fun() ->
        Proxy = {10, 0, 0, 2},
        Client = fun(Header) -> blog_rate_limit:client_ip(req(Proxy, Header)) end,
        ?assertEqual({203, 0, 113, 7}, Client(~"203.0.113.7:5678")),
        ?assertEqual({16#2001, 16#db8, 0, 0, 0, 0, 0, 1}, Client(~"[2001:db8::1]:443")),
        ?assertEqual({16#2001, 16#db8, 0, 0, 0, 0, 0, 1}, Client(~"[2001:db8::1]")),
        ?assertEqual({203, 0, 113, 7}, Client(~"203.0.113.7, 10.0.0.2:80")),
        ?assertEqual({203, 0, 113, 7}, Client(~"203.0.113.7,")),
        ?assertEqual({203, 0, 113, 7}, Client(~"203.0.113.7, , ")),
        ?assertEqual(Proxy, Client(~"203.0.113.7:")),
        ?assertEqual(Proxy, Client(~"203.0.113.7:80x")),
        ?assertEqual(Proxy, Client(~"[2001:db8::1]x"))
    end).

a_mistyped_trusted_proxy_fails_loudly(_Config) ->
    Req = req({10, 0, 0, 2}, ~"203.0.113.7"),
    [
        with_trusted(Trusted, fun() ->
            ?assertError({invalid_trusted_proxy, _, _}, blog_rate_limit:client_ip(Req))
        end)
     || Trusted <- [
            ["10.0.0.2"],
            "10.0.0.2",
            {10, 0, 0, 2},
            [{10, 0, 0, 256}],
            [{{10, 0, 0, 0}, -1}],
            [{{10, 0, 0, 0}, 33}],
            [{{16#fd00, 0, 0, 0, 0, 0, 0, 0}, 129}],
            [{"10.0.0.0", 8}]
        ]
    ],
    with_trusted([{{10, 0, 0, 2}, 32}], fun() ->
        ?assertEqual({203, 0, 113, 7}, blog_rate_limit:client_ip(Req))
    end).

%% seki logs a warning on every new_limiter call for a name it already has.
account_limiter_is_created_once(_Config) ->
    ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{pid => self()}}),
    try
        Emails = [
            iolist_to_binary([~"once-", integer_to_list(N), ~"@example.com"])
         || N <- lists:seq(1, 5)
        ],
        ?assertEqual(lists:duplicate(5, allow), [blog_rate_limit:check_account(E) || E <- Emails]),
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
%% Helpers
%%====================================================================

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

with_trusted(Trusted, Fun) ->
    application:set_env(blog, trusted_proxies, Trusted),
    try
        Fun()
    after
        application:unset_env(blog, trusted_proxies)
    end.

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
