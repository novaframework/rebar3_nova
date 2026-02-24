-module(rebar3_nova_gen_auth).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen_auth).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {namespace, nova},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 nova gen_auth"},
            {opts, []},
            {short_desc, "Generate email/password authentication"},
            {desc, "Generates schemas, controllers, and context modules for email/password auth"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    AppName = rebar3_nova_utils:get_app_name(State),
    AppDir = rebar3_nova_utils:get_app_dir(State),
    App = atom_to_list(AppName),
    generate_migration(AppDir),
    generate_user_schema(App, AppDir),
    generate_user_token_schema(App, AppDir),
    generate_accounts(App, AppDir),
    generate_auth(App, AppDir),
    generate_session_controller(App, AppDir),
    generate_registration_controller(App, AppDir),
    generate_user_controller(App, AppDir),
    generate_test_suite(App, AppDir),
    print_instructions(App),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%======================================================================
%% Internal: timestamp for migration filename
%%======================================================================

timestamp() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    lists:flatten(io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
                                [Y, Mo, D, H, Mi, S])).

%%======================================================================
%% Migration
%%======================================================================

generate_migration(AppDir) ->
    TS = timestamp(),
    Mod = "m" ++ TS ++ "_create_auth_tables",
    FileName = filename:join([AppDir, "src", "migrations", Mod ++ ".erl"]),
    Content = [
        "-module(", Mod, ").\n"
        "-behaviour(kura_migration).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([up/0, down/0]).\n\n"
        "up() ->\n"
        "    [{execute, <<\"CREATE EXTENSION IF NOT EXISTS citext\">>},\n"
        "     {create_table, <<\"users\">>, [\n"
        "         #kura_column{name = id, type = id, primary_key = true},\n"
        "         #kura_column{name = email, type = string, nullable = false},\n"
        "         #kura_column{name = hashed_password, type = string, nullable = false},\n"
        "         #kura_column{name = confirmed_at, type = utc_datetime},\n"
        "         #kura_column{name = inserted_at, type = utc_datetime},\n"
        "         #kura_column{name = updated_at, type = utc_datetime}\n"
        "     ]},\n"
        "     {create_index, <<\"users_email_index\">>, <<\"users\">>, [email], [unique]},\n"
        "     {create_table, <<\"user_tokens\">>, [\n"
        "         #kura_column{name = id, type = id, primary_key = true},\n"
        "         #kura_column{name = user_id, type = integer, nullable = false},\n"
        "         #kura_column{name = token, type = string, nullable = false},\n"
        "         #kura_column{name = context, type = string, nullable = false},\n"
        "         #kura_column{name = inserted_at, type = utc_datetime}\n"
        "     ]},\n"
        "     {create_index, <<\"user_tokens_user_id_index\">>, <<\"user_tokens\">>, [user_id], []},\n"
        "     {create_index, <<\"user_tokens_context_token_index\">>, <<\"user_tokens\">>,\n"
        "         [context, token], [unique]},\n"
        "     {execute, <<\"ALTER TABLE user_tokens ADD CONSTRAINT user_tokens_user_id_fkey \"\n"
        "                 \"FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE\">>}\n"
        "    ].\n\n"
        "down() ->\n"
        "    [{drop_table, <<\"user_tokens\">>},\n"
        "     {drop_table, <<\"users\">>}].\n"
    ],
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% User schema
%%======================================================================

generate_user_schema(App, AppDir) ->
    Mod = App ++ "_user",
    FileName = filename:join([AppDir, "src", "schemas", Mod ++ ".erl"]),
    Content = [
        "-module(", Mod, ").\n"
        "-behaviour(kura_schema).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([table/0, fields/0, primary_key/0]).\n"
        "-export([registration_changeset/2, password_changeset/2, email_changeset/2]).\n\n"
        "table() -> <<\"users\">>.\n\n"
        "primary_key() -> id.\n\n"
        "fields() ->\n"
        "    [#kura_field{name = id, type = id, primary_key = true, nullable = false},\n"
        "     #kura_field{name = email, type = string, nullable = false},\n"
        "     #kura_field{name = hashed_password, type = string, nullable = false},\n"
        "     #kura_field{name = confirmed_at, type = utc_datetime},\n"
        "     #kura_field{name = inserted_at, type = utc_datetime},\n"
        "     #kura_field{name = updated_at, type = utc_datetime},\n"
        "     #kura_field{name = password, type = string, virtual = true},\n"
        "     #kura_field{name = password_confirmation, type = string, virtual = true}].\n\n"
        "registration_changeset(Data, Params) ->\n"
        "    CS = kura_changeset:cast(", Mod, ", Data, Params,\n"
        "             [email, password, password_confirmation]),\n"
        "    CS1 = kura_changeset:validate_required(CS, [email, password, password_confirmation]),\n"
        "    CS2 = kura_changeset:validate_format(CS1, email, <<\"^[^@\\\\s]+@[^@\\\\s]+$\">>),\n"
        "    CS3 = kura_changeset:validate_length(CS2, email, [{max, 160}]),\n"
        "    CS4 = kura_changeset:validate_length(CS3, password, [{min, 12}, {max, 72}]),\n"
        "    CS5 = validate_password_confirmation(CS4),\n"
        "    CS6 = maybe_hash_password(CS5),\n"
        "    kura_changeset:unique_constraint(CS6, email).\n\n"
        "password_changeset(Data, Params) ->\n"
        "    CS = kura_changeset:cast(", Mod, ", Data, Params,\n"
        "             [password, password_confirmation]),\n"
        "    CS1 = kura_changeset:validate_required(CS, [password, password_confirmation]),\n"
        "    CS2 = kura_changeset:validate_length(CS1, password, [{min, 12}, {max, 72}]),\n"
        "    CS3 = validate_password_confirmation(CS2),\n"
        "    maybe_hash_password(CS3).\n\n"
        "email_changeset(Data, Params) ->\n"
        "    CS = kura_changeset:cast(", Mod, ", Data, Params, [email]),\n"
        "    CS1 = kura_changeset:validate_required(CS, [email]),\n"
        "    CS2 = kura_changeset:validate_format(CS1, email, <<\"^[^@\\\\s]+@[^@\\\\s]+$\">>),\n"
        "    CS3 = kura_changeset:validate_length(CS2, email, [{max, 160}]),\n"
        "    kura_changeset:unique_constraint(CS3, email).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Internal\n"
        "%%----------------------------------------------------------------------\n\n"
        "validate_password_confirmation(CS) ->\n"
        "    case {kura_changeset:get_change(CS, password),\n"
        "          kura_changeset:get_change(CS, password_confirmation)} of\n"
        "        {Pass, Pass} when Pass =/= undefined -> CS;\n"
        "        {undefined, _} -> CS;\n"
        "        _ -> kura_changeset:add_error(CS, password_confirmation,\n"
        "                 <<\"does not match password\">>)\n"
        "    end.\n\n"
        "maybe_hash_password(#kura_changeset{valid = true} = CS) ->\n"
        "    case kura_changeset:get_change(CS, password) of\n"
        "        undefined -> CS;\n"
        "        Password ->\n"
        "            Hashed = list_to_binary(\n"
        "                bcrypt:hashpw(binary_to_list(Password), bcrypt:gen_salt())),\n"
        "            kura_changeset:put_change(CS, hashed_password, Hashed)\n"
        "    end;\n"
        "maybe_hash_password(CS) ->\n"
        "    CS.\n"
    ],
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% User token schema
%%======================================================================

generate_user_token_schema(App, AppDir) ->
    Mod = App ++ "_user_token",
    UserMod = App ++ "_user",
    FileName = filename:join([AppDir, "src", "schemas", Mod ++ ".erl"]),
    Content = [
        "-module(", Mod, ").\n"
        "-behaviour(kura_schema).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([table/0, fields/0, primary_key/0, associations/0]).\n\n"
        "table() -> <<\"user_tokens\">>.\n\n"
        "primary_key() -> id.\n\n"
        "fields() ->\n"
        "    [#kura_field{name = id, type = id, primary_key = true, nullable = false},\n"
        "     #kura_field{name = user_id, type = integer, nullable = false},\n"
        "     #kura_field{name = token, type = string, nullable = false},\n"
        "     #kura_field{name = context, type = string, nullable = false},\n"
        "     #kura_field{name = inserted_at, type = utc_datetime}].\n\n"
        "associations() ->\n"
        "    [#kura_assoc{name = user, type = belongs_to, schema = ", UserMod, ",\n"
        "                 foreign_key = user_id}].\n"
    ],
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% Accounts context
%%======================================================================

generate_accounts(App, AppDir) ->
    Mod = App ++ "_accounts",
    Repo = App ++ "_repo",
    UserMod = App ++ "_user",
    TokenMod = App ++ "_user_token",
    FileName = filename:join([AppDir, "src", Mod ++ ".erl"]),
    Content = [
        "-module(", Mod, ").\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([\n"
        "    register_user/1,\n"
        "    get_user_by_email_and_password/2,\n"
        "    get_user_by_id/1,\n"
        "    generate_session_token/1,\n"
        "    get_user_by_session_token/1,\n"
        "    delete_session_token/1,\n"
        "    delete_all_user_tokens/1,\n"
        "    change_user_password/3,\n"
        "    change_user_email/3,\n"
        "    user_to_json/1,\n"
        "    format_errors/1\n"
        "]).\n\n"
        "-define(SESSION_VALIDITY_DAYS, 14).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Registration\n"
        "%%----------------------------------------------------------------------\n\n"
        "register_user(Params) ->\n"
        "    Now = calendar:universal_time(),\n"
        "    CS = ", UserMod, ":registration_changeset(#{}, Params),\n"
        "    CS1 = kura_changeset:put_change(CS, inserted_at, Now),\n"
        "    CS2 = kura_changeset:put_change(CS1, updated_at, Now),\n"
        "    ", Repo, ":insert(CS2).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Authentication\n"
        "%%----------------------------------------------------------------------\n\n"
        "get_user_by_email_and_password(Email, Password) ->\n"
        "    Q = kura_query:where(kura_query:from(", UserMod, "), {email, Email}),\n"
        "    case ", Repo, ":all(Q) of\n"
        "        {ok, [User]} ->\n"
        "            case verify_password(Password, maps:get(hashed_password, User)) of\n"
        "                true -> {ok, User};\n"
        "                false -> {error, invalid_credentials}\n"
        "            end;\n"
        "        _ ->\n"
        "            dummy_verify(),\n"
        "            {error, invalid_credentials}\n"
        "    end.\n\n"
        "get_user_by_id(Id) ->\n"
        "    case ", Repo, ":get(", UserMod, ", Id) of\n"
        "        {ok, User} -> {ok, User};\n"
        "        _ -> {error, not_found}\n"
        "    end.\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Session tokens\n"
        "%%----------------------------------------------------------------------\n\n"
        "generate_session_token(User) ->\n"
        "    Raw = crypto:strong_rand_bytes(32),\n"
        "    SessionToken = base64:encode(Raw),\n"
        "    HashedToken = base64:encode(crypto:hash(sha256, Raw)),\n"
        "    Now = calendar:universal_time(),\n"
        "    CS = kura_changeset:cast(", TokenMod, ", #{}, #{user_id => maps:get(id, User),\n"
        "        token => HashedToken, context => <<\"session\">>,\n"
        "        inserted_at => Now}, [user_id, token, context, inserted_at]),\n"
        "    case ", Repo, ":insert(CS) of\n"
        "        {ok, _} -> {ok, SessionToken};\n"
        "        {error, _} = Err -> Err\n"
        "    end.\n\n"
        "get_user_by_session_token(SessionToken) ->\n"
        "    try\n"
        "        Raw = base64:decode(SessionToken),\n"
        "        HashedToken = base64:encode(crypto:hash(sha256, Raw)),\n"
        "        Q = kura_query:where(\n"
        "                kura_query:where(kura_query:from(", TokenMod, "),\n"
        "                    {token, HashedToken}),\n"
        "                {context, <<\"session\">>}),\n"
        "        case ", Repo, ":all(Q) of\n"
        "            {ok, [Token]} ->\n"
        "                case token_valid(maps:get(inserted_at, Token)) of\n"
        "                    true -> get_user_by_id(maps:get(user_id, Token));\n"
        "                    false -> {error, token_expired}\n"
        "                end;\n"
        "            _ -> {error, not_found}\n"
        "        end\n"
        "    catch\n"
        "        _:_ -> {error, invalid_token}\n"
        "    end.\n\n"
        "delete_session_token(SessionToken) ->\n"
        "    try\n"
        "        Raw = base64:decode(SessionToken),\n"
        "        HashedToken = base64:encode(crypto:hash(sha256, Raw)),\n"
        "        Q = kura_query:where(\n"
        "                kura_query:where(kura_query:from(", TokenMod, "),\n"
        "                    {token, HashedToken}),\n"
        "                {context, <<\"session\">>}),\n"
        "        ", Repo, ":delete_all(Q),\n"
        "        ok\n"
        "    catch\n"
        "        _:_ -> ok\n"
        "    end.\n\n"
        "delete_all_user_tokens(UserId) ->\n"
        "    Q = kura_query:where(kura_query:from(", TokenMod, "), {user_id, UserId}),\n"
        "    ", Repo, ":delete_all(Q),\n"
        "    ok.\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Password & email changes\n"
        "%%----------------------------------------------------------------------\n\n"
        "change_user_password(User, CurrentPassword, NewParams) ->\n"
        "    case verify_password(CurrentPassword, maps:get(hashed_password, User)) of\n"
        "        true ->\n"
        "            Now = calendar:universal_time(),\n"
        "            CS = ", UserMod, ":password_changeset(User, NewParams),\n"
        "            CS1 = kura_changeset:put_change(CS, updated_at, Now),\n"
        "            case ", Repo, ":update(CS1) of\n"
        "                {ok, UpdatedUser} ->\n"
        "                    delete_all_user_tokens(maps:get(id, User)),\n"
        "                    {ok, UpdatedUser};\n"
        "                {error, _} = Err -> Err\n"
        "            end;\n"
        "        false ->\n"
        "            {error, invalid_password}\n"
        "    end.\n\n"
        "change_user_email(User, CurrentPassword, NewParams) ->\n"
        "    case verify_password(CurrentPassword, maps:get(hashed_password, User)) of\n"
        "        true ->\n"
        "            Now = calendar:universal_time(),\n"
        "            CS = ", UserMod, ":email_changeset(User, NewParams),\n"
        "            CS1 = kura_changeset:put_change(CS, updated_at, Now),\n"
        "            case ", Repo, ":update(CS1) of\n"
        "                {ok, UpdatedUser} ->\n"
        "                    delete_all_user_tokens(maps:get(id, User)),\n"
        "                    {ok, UpdatedUser};\n"
        "                {error, _} = Err -> Err\n"
        "            end;\n"
        "        false ->\n"
        "            {error, invalid_password}\n"
        "    end.\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% JSON helpers\n"
        "%%----------------------------------------------------------------------\n\n"
        "user_to_json(User) ->\n"
        "    #{<<\"id\">> => maps:get(id, User),\n"
        "      <<\"email\">> => maps:get(email, User)}.\n\n"
        "format_errors(#kura_changeset{errors = Errors}) ->\n"
        "    maps:from_list([{atom_to_binary(F), M} || {F, M} <- Errors]).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Internal\n"
        "%%----------------------------------------------------------------------\n\n"
        "verify_password(Password, HashedPassword) ->\n"
        "    Hash = list_to_binary(\n"
        "        bcrypt:hashpw(binary_to_list(Password), binary_to_list(HashedPassword))),\n"
        "    crypto:hash_equals(Hash, HashedPassword).\n\n"
        "dummy_verify() ->\n"
        "    bcrypt:hashpw(\"dummy\", bcrypt:gen_salt()),\n"
        "    false.\n\n"
        "token_valid(InsertedAt) ->\n"
        "    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),\n"
        "    TokenTime = calendar:datetime_to_gregorian_seconds(InsertedAt),\n"
        "    (Now - TokenTime) < (?SESSION_VALIDITY_DAYS * 24 * 60 * 60).\n"
    ],
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% Auth security module
%%======================================================================

generate_auth(App, AppDir) ->
    Mod = App ++ "_auth",
    Accounts = App ++ "_accounts",
    FileName = filename:join([AppDir, "src", Mod ++ ".erl"]),
    Content = [
        "-module(", Mod, ").\n\n"
        "-export([require_authenticated/1]).\n\n"
        "require_authenticated(Req) ->\n"
        "    case nova_session:get(Req, <<\"session_token\">>) of\n"
        "        {ok, Token} ->\n"
        "            case ", Accounts, ":get_user_by_session_token(Token) of\n"
        "                {ok, User} ->\n"
        "                    {true, User};\n"
        "                _ ->\n"
        "                    unauthorized()\n"
        "            end;\n"
        "        _ ->\n"
        "            unauthorized()\n"
        "    end.\n\n"
        "unauthorized() ->\n"
        "    Body = thoas:encode(#{<<\"error\">> => <<\"unauthorized\">>}),\n"
        "    {false, 401, #{<<\"content-type\">> => <<\"application/json\">>}, Body}.\n"
    ],
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% Session controller
%%======================================================================

generate_session_controller(App, AppDir) ->
    Mod = App ++ "_session_controller",
    Accounts = App ++ "_accounts",
    FileName = filename:join([AppDir, "src", "controllers", Mod ++ ".erl"]),
    Content = [
        "-module(", Mod, ").\n\n"
        "-export([create/1, delete/1]).\n\n"
        "create(Req) ->\n"
        "    #{<<\"email\">> := Email, <<\"password\">> := Password} = maps:get(json, Req),\n"
        "    case ", Accounts, ":get_user_by_email_and_password(Email, Password) of\n"
        "        {ok, User} ->\n"
        "            {ok, Token} = ", Accounts, ":generate_session_token(User),\n"
        "            ok = nova_session:set(Req, <<\"session_token\">>, Token),\n"
        "            {json, #{<<\"user\">> => ", Accounts, ":user_to_json(User)}};\n"
        "        {error, _} ->\n"
        "            {json, 401, #{}, #{<<\"error\">> => <<\"invalid email or password\">>}}\n"
        "    end.\n\n"
        "delete(Req) ->\n"
        "    case nova_session:get(Req, <<\"session_token\">>) of\n"
        "        {ok, Token} ->\n"
        "            ", Accounts, ":delete_session_token(Token);\n"
        "        _ ->\n"
        "            ok\n"
        "    end,\n"
        "    nova_session:delete(Req, <<\"session_token\">>),\n"
        "    {status, 204}.\n"
    ],
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% Registration controller
%%======================================================================

generate_registration_controller(App, AppDir) ->
    Mod = App ++ "_registration_controller",
    Accounts = App ++ "_accounts",
    FileName = filename:join([AppDir, "src", "controllers", Mod ++ ".erl"]),
    Content = [
        "-module(", Mod, ").\n\n"
        "-export([create/1]).\n\n"
        "create(Req) ->\n"
        "    Params = maps:get(json, Req),\n"
        "    case ", Accounts, ":register_user(Params) of\n"
        "        {ok, User} ->\n"
        "            {ok, Token} = ", Accounts, ":generate_session_token(User),\n"
        "            ok = nova_session:set(Req, <<\"session_token\">>, Token),\n"
        "            {json, 201, #{}, #{<<\"user\">> => ", Accounts, ":user_to_json(User)}};\n"
        "        {error, CS} ->\n"
        "            {json, 422, #{}, #{<<\"errors\">> => ", Accounts, ":format_errors(CS)}}\n"
        "    end.\n"
    ],
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% User controller
%%======================================================================

generate_user_controller(App, AppDir) ->
    Mod = App ++ "_user_controller",
    Accounts = App ++ "_accounts",
    FileName = filename:join([AppDir, "src", "controllers", Mod ++ ".erl"]),
    Content = [
        "-module(", Mod, ").\n\n"
        "-export([show/1, update_password/1, update_email/1]).\n\n"
        "show(Req) ->\n"
        "    User = maps:get(auth_data, Req),\n"
        "    {json, #{<<\"user\">> => ", Accounts, ":user_to_json(User)}}.\n\n"
        "update_password(Req) ->\n"
        "    User = maps:get(auth_data, Req),\n"
        "    #{<<\"current_password\">> := CurrentPassword} = maps:get(json, Req),\n"
        "    NewParams = maps:get(json, Req),\n"
        "    case ", Accounts, ":change_user_password(User, CurrentPassword, NewParams) of\n"
        "        {ok, UpdatedUser} ->\n"
        "            {ok, Token} = ", Accounts, ":generate_session_token(UpdatedUser),\n"
        "            ok = nova_session:set(Req, <<\"session_token\">>, Token),\n"
        "            {json, #{<<\"user\">> => ", Accounts, ":user_to_json(UpdatedUser)}};\n"
        "        {error, invalid_password} ->\n"
        "            {json, 401, #{}, #{<<\"error\">> => <<\"invalid current password\">>}};\n"
        "        {error, CS} ->\n"
        "            {json, 422, #{}, #{<<\"errors\">> => ", Accounts, ":format_errors(CS)}}\n"
        "    end.\n\n"
        "update_email(Req) ->\n"
        "    User = maps:get(auth_data, Req),\n"
        "    #{<<\"current_password\">> := CurrentPassword} = maps:get(json, Req),\n"
        "    NewParams = maps:get(json, Req),\n"
        "    case ", Accounts, ":change_user_email(User, CurrentPassword, NewParams) of\n"
        "        {ok, UpdatedUser} ->\n"
        "            {ok, Token} = ", Accounts, ":generate_session_token(UpdatedUser),\n"
        "            ok = nova_session:set(Req, <<\"session_token\">>, Token),\n"
        "            {json, #{<<\"user\">> => ", Accounts, ":user_to_json(UpdatedUser)}};\n"
        "        {error, invalid_password} ->\n"
        "            {json, 401, #{}, #{<<\"error\">> => <<\"invalid current password\">>}};\n"
        "        {error, CS} ->\n"
        "            {json, 422, #{}, #{<<\"errors\">> => ", Accounts, ":format_errors(CS)}}\n"
        "    end.\n"
    ],
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% Test suite
%%======================================================================

generate_test_suite(App, AppDir) ->
    Suite = App ++ "_auth_SUITE",
    FileName = filename:join([AppDir, "test", Suite ++ ".erl"]),
    Content = [
        "-module(", Suite, ").\n"
        "-include_lib(\"common_test/include/ct.hrl\").\n\n"
        "-export([all/0, init_per_suite/1, end_per_suite/1,\n"
        "         init_per_testcase/2, end_per_testcase/2]).\n"
        "-export([\n"
        "    test_register/1,\n"
        "    test_register_invalid/1,\n"
        "    test_login/1,\n"
        "    test_login_invalid/1,\n"
        "    test_logout/1,\n"
        "    test_get_current_user/1,\n"
        "    test_unauthorized/1,\n"
        "    test_update_password/1,\n"
        "    test_update_email/1\n"
        "]).\n\n"
        "-define(BASE_URL, \"http://localhost:8080\").\n\n"
        "all() ->\n"
        "    [test_register, test_register_invalid, test_login, test_login_invalid,\n"
        "     test_logout, test_get_current_user, test_unauthorized,\n"
        "     test_update_password, test_update_email].\n\n"
        "init_per_suite(Config) ->\n"
        "    application:ensure_all_started(inets),\n"
        "    application:ensure_all_started(ssl),\n"
        "    application:ensure_all_started(", App, "),\n"
        "    Config.\n\n"
        "end_per_suite(_Config) ->\n"
        "    application:stop(", App, "),\n"
        "    ok.\n\n"
        "init_per_testcase(_TestCase, Config) ->\n"
        "    Config.\n\n"
        "end_per_testcase(_TestCase, _Config) ->\n"
        "    ok.\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Registration\n"
        "%%----------------------------------------------------------------------\n\n"
        "test_register(_Config) ->\n"
        "    Body = encode(#{<<\"email\">> => <<\"register@example.com\">>,\n"
        "                    <<\"password\">> => <<\"password123456\">>,\n"
        "                    <<\"password_confirmation\">> => <<\"password123456\">>}),\n"
        "    {ok, {{_, 201, _}, _, RespBody}} =\n"
        "        httpc:request(post,\n"
        "            {?BASE_URL ++ \"/api/register\", [], \"application/json\", Body},\n"
        "            [], []),\n"
        "    #{<<\"user\">> := #{<<\"id\">> := _, <<\"email\">> := <<\"register@example.com\">>}} =\n"
        "        decode(RespBody).\n\n"
        "test_register_invalid(_Config) ->\n"
        "    %% Missing password\n"
        "    Body1 = encode(#{<<\"email\">> => <<\"invalid@example.com\">>}),\n"
        "    {ok, {{_, 422, _}, _, _}} =\n"
        "        httpc:request(post,\n"
        "            {?BASE_URL ++ \"/api/register\", [], \"application/json\", Body1},\n"
        "            [], []),\n"
        "    %% Short password\n"
        "    Body2 = encode(#{<<\"email\">> => <<\"invalid@example.com\">>,\n"
        "                     <<\"password\">> => <<\"short\">>,\n"
        "                     <<\"password_confirmation\">> => <<\"short\">>}),\n"
        "    {ok, {{_, 422, _}, _, _}} =\n"
        "        httpc:request(post,\n"
        "            {?BASE_URL ++ \"/api/register\", [], \"application/json\", Body2},\n"
        "            [], []).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Login / Logout\n"
        "%%----------------------------------------------------------------------\n\n"
        "test_login(_Config) ->\n"
        "    register_user(<<\"login@example.com\">>, <<\"password123456\">>),\n"
        "    Body = encode(#{<<\"email\">> => <<\"login@example.com\">>,\n"
        "                    <<\"password\">> => <<\"password123456\">>}),\n"
        "    {ok, {{_, 200, _}, _, RespBody}} =\n"
        "        httpc:request(post,\n"
        "            {?BASE_URL ++ \"/api/login\", [], \"application/json\", Body},\n"
        "            [], []),\n"
        "    #{<<\"user\">> := #{<<\"email\">> := <<\"login@example.com\">>}} =\n"
        "        decode(RespBody).\n\n"
        "test_login_invalid(_Config) ->\n"
        "    Body = encode(#{<<\"email\">> => <<\"nobody@example.com\">>,\n"
        "                    <<\"password\">> => <<\"wrongpassword1\">>}),\n"
        "    {ok, {{_, 401, _}, _, _}} =\n"
        "        httpc:request(post,\n"
        "            {?BASE_URL ++ \"/api/login\", [], \"application/json\", Body},\n"
        "            [], []).\n\n"
        "test_logout(_Config) ->\n"
        "    Cookie = register_and_login(<<\"logout@example.com\">>, <<\"password123456\">>),\n"
        "    {ok, {{_, 204, _}, _, _}} =\n"
        "        httpc:request(delete,\n"
        "            {?BASE_URL ++ \"/api/logout\", [{\"Cookie\", Cookie}]},\n"
        "            [], []).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Current user\n"
        "%%----------------------------------------------------------------------\n\n"
        "test_get_current_user(_Config) ->\n"
        "    Cookie = register_and_login(<<\"me@example.com\">>, <<\"password123456\">>),\n"
        "    {ok, {{_, 200, _}, _, RespBody}} =\n"
        "        httpc:request(get,\n"
        "            {?BASE_URL ++ \"/api/me\", [{\"Cookie\", Cookie}]},\n"
        "            [], []),\n"
        "    #{<<\"user\">> := #{<<\"email\">> := <<\"me@example.com\">>}} =\n"
        "        decode(RespBody).\n\n"
        "test_unauthorized(_Config) ->\n"
        "    {ok, {{_, 401, _}, _, _}} =\n"
        "        httpc:request(get, {?BASE_URL ++ \"/api/me\", []}, [], []).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Password & email update\n"
        "%%----------------------------------------------------------------------\n\n"
        "test_update_password(_Config) ->\n"
        "    Cookie = register_and_login(<<\"pwchange@example.com\">>, <<\"password123456\">>),\n"
        "    Body = encode(#{<<\"current_password\">> => <<\"password123456\">>,\n"
        "                    <<\"password\">> => <<\"newpassword12345\">>,\n"
        "                    <<\"password_confirmation\">> => <<\"newpassword12345\">>}),\n"
        "    {ok, {{_, 200, _}, _, _}} =\n"
        "        httpc:request(put,\n"
        "            {?BASE_URL ++ \"/api/me/password\",\n"
        "             [{\"Cookie\", Cookie}], \"application/json\", Body},\n"
        "            [], []).\n\n"
        "test_update_email(_Config) ->\n"
        "    Cookie = register_and_login(<<\"emailchange@example.com\">>, <<\"password123456\">>),\n"
        "    Body = encode(#{<<\"current_password\">> => <<\"password123456\">>,\n"
        "                    <<\"email\">> => <<\"newemail@example.com\">>}),\n"
        "    {ok, {{_, 200, _}, _, _}} =\n"
        "        httpc:request(put,\n"
        "            {?BASE_URL ++ \"/api/me/email\",\n"
        "             [{\"Cookie\", Cookie}], \"application/json\", Body},\n"
        "            [], []).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Helpers\n"
        "%%----------------------------------------------------------------------\n\n"
        "register_user(Email, Password) ->\n"
        "    Body = encode(#{<<\"email\">> => Email, <<\"password\">> => Password,\n"
        "                    <<\"password_confirmation\">> => Password}),\n"
        "    {ok, {{_, 201, _}, _, _}} =\n"
        "        httpc:request(post,\n"
        "            {?BASE_URL ++ \"/api/register\", [], \"application/json\", Body},\n"
        "            [], []).\n\n"
        "register_and_login(Email, Password) ->\n"
        "    Body = encode(#{<<\"email\">> => Email, <<\"password\">> => Password,\n"
        "                    <<\"password_confirmation\">> => Password}),\n"
        "    {ok, {{_, 201, _}, Headers, _}} =\n"
        "        httpc:request(post,\n"
        "            {?BASE_URL ++ \"/api/register\", [], \"application/json\", Body},\n"
        "            [], []),\n"
        "    extract_cookie(Headers).\n\n"
        "extract_cookie(Headers) ->\n"
        "    case lists:keyfind(\"set-cookie\", 1, Headers) of\n"
        "        {_, Cookie} -> Cookie;\n"
        "        false -> \"\"\n"
        "    end.\n\n"
        "encode(Map) ->\n"
        "    binary_to_list(thoas:encode(Map)).\n\n"
        "decode(Body) ->\n"
        "    {ok, Json} = thoas:decode(list_to_binary(Body)),\n"
        "    Json.\n"
    ],
    rebar3_nova_utils:write_file_if_not_exists(FileName, Content).

%%======================================================================
%% Print instructions
%%======================================================================

print_instructions(App) ->
    AuthMod = App ++ "_auth",
    SessionCtrl = App ++ "_session_controller",
    RegCtrl = App ++ "_registration_controller",
    UserCtrl = App ++ "_user_controller",
    rebar_api:info("~n==> Authentication files generated successfully!~n", []),
    rebar_api:info("Next steps:~n", []),
    rebar_api:info("1. Add kura and bcrypt to your deps in rebar.config:~n", []),
    rebar_api:info("   {deps, [..., kura, bcrypt]}.~n~n", []),
    rebar_api:info("2. Add these routes to your router:~n", []),
    rebar_api:info("   %% Public routes~n", []),
    rebar_api:info("   #{prefix => <<\"/api\">>,~n"
                   "     security => false,~n"
                   "     plugins => [{pre_request, nova_request_plugin,~n"
                   "                  #{decode_json_body => true}}],~n"
                   "     routes => [~n"
                   "       {<<\"/register\">>, fun ~s:create/1, #{methods => [post]}},~n"
                   "       {<<\"/login\">>, fun ~s:create/1, #{methods => [post]}}~n"
                   "     ]}~n", [RegCtrl, SessionCtrl]),
    rebar_api:info("   %% Protected routes~n", []),
    rebar_api:info("   #{prefix => <<\"/api\">>,~n"
                   "     security => fun ~s:require_authenticated/1,~n"
                   "     plugins => [{pre_request, nova_request_plugin,~n"
                   "                  #{decode_json_body => true}}],~n"
                   "     routes => [~n"
                   "       {<<\"/logout\">>, fun ~s:delete/1, #{methods => [delete]}},~n"
                   "       {<<\"/me\">>, fun ~s:show/1, #{methods => [get]}},~n"
                   "       {<<\"/me/password\">>, fun ~s:update_password/1, #{methods => [put]}},~n"
                   "       {<<\"/me/email\">>, fun ~s:update_email/1, #{methods => [put]}}~n"
                   "     ]}~n", [AuthMod, SessionCtrl, UserCtrl, UserCtrl, UserCtrl]),
    rebar_api:info("3. Add src/schemas and src/migrations to src_dirs in rebar.config:~n", []),
    rebar_api:info("   {src_dirs, [\"src\", \"src/controllers\", \"src/schemas\", \"src/migrations\"]}.~n~n", []),
    rebar_api:info("4. Run the migration:~n", []),
    rebar_api:info("   rebar3 kura migrate~n~n", []),
    rebar_api:info("5. Ensure nova_request_plugin with decode_json_body is configured~n"
                   "   (either globally in sys.config or per route group as shown above).~n", []).
