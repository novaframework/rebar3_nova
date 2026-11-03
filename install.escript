#!/usr/bin/env escript
%% -*- erlang -*-

-include_lib("kernel/include/file.hrl").

-define(TERM_RED, "\033[31m").
-define(TERM_GREEN, "\033[92m").
-define(TERM_YELLOW, "\033[33m").
-define(TERM_BLUE, "\033[96m").
-define(TERM_BOLD, "\033[1m").
-define(TERM_RESET, "\033[m").

-define(NOVA_PLUGIN, "{rebar3_nova, \"~> 0.13\"}").

%% The rebar3 release the installer fetches, and the sha256 GitHub publishes
%% for that asset. The download is only run when it matches.
-define(REBAR3_URL, "https://github.com/erlang/rebar3/releases/download/3.27.0/rebar3").
-define(REBAR3_SHA256, "af85aab41f9fd74bdd6341ebdf6fe9c88077aab9f8eac82371583fa02f2b0bdf").

-define(NOVA_LOGO, ["  _   _                    __                                             _    ",
                    " | \\ | | _____   ____ _   / _|_ __ __ _ _ __ ___   _____      _____  _ __| | __",
                    " |  \\| |/ _ \\ \\ / / _` | | |_| '__/ _` | '_ ` _ \\ / _ \\ \\ /\\ / / _ \\| '__| |/ /",
                    " | |\\  | (_) \\ V / (_| | |  _| | | (_| | | | | | |  __/\\ V  V / (_) | |  |   < ",
                    " |_| \\_|\\___/ \\_/ \\__,_| |_| |_|  \\__,_|_| |_| |_|\\___| \\_/\\_/ \\___/|_|  |_|\\_\\",
                    "                       http://www.novaframework.org"]).

print(Mode, TextList) ->
    io:format("~s", [Mode]),
    print_lines(TextList),
    io:format("~s", [?TERM_RESET]).

print_lines([]) -> [];
print_lines([Hd|Tl]) ->
    io:format("~s~n", [Hd]),
    print_lines(Tl).

prompt_yesno(Message) ->
    case io:fread(standard_io, Message, "~s") of
        {ok, [Yes]} when Yes == "Y" orelse Yes == "y" ->
            yes;
        {ok, [No]} when No == "N" orelse No == "n" ->
            no;
        P ->
            io:format("GOT: ~p~n", [P]),
            prompt_yesno(Message)
    end.

check_for_rebar3() ->
    case os:cmd("command -v rebar3") of
        [] ->
            print(?TERM_YELLOW, ["Rebar3 could not be found in your $PATH. This might be cause you are", "running a local installation or forgot to install it."]),
            case prompt_yesno("Do you want to install rebar3? (Y/n)") of
                yes ->
                    case install_rebar3() of
                        ok ->
                            print(?TERM_BLUE, ["Rebar3 is now installed. " ++ ?TERM_BOLD ++ "Remember to set your $PATH: export PATH=" ++ os:getenv("HOME") ++ "/.cache/rebar3/bin:$PATH"]),
                            print(?TERM_BLUE, ["Continues with the installation"]),
                            ok;
                        {error, Reason} ->
                            print(?TERM_RED, [io_lib:format("Installing rebar3 failed: ~p", [Reason])]),
                            {error, Reason}
                    end;
                no ->
                    print(?TERM_RED, ["Rebar3 is required by nova_plugins."]),
                    {error, not_installed}
            end;
        _ ->
            ok
    end.

%% Downloads into a fresh private directory, so neither a rebar3 already in
%% the working directory nor a file planted in a shared one is ever run.
install_rebar3() ->
    Dir = filename:join(temp_root(), "rebar3_nova_install_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    ok = file:change_mode(Dir, 8#700),
    Rebar3 = filename:join(Dir, "rebar3"),
    try
        case download(?REBAR3_URL) of
            {ok, Bin} ->
                case sha256(Bin) of
                    ?REBAR3_SHA256 ->
                        ok = file:write_file(Rebar3, Bin),
                        ok = file:change_mode(Rebar3, 8#700),
                        run(Rebar3, ["local", "install"]);
                    Other ->
                        {error, {checksum_mismatch, Other}}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    after
        file:del_dir_r(Dir)
    end.

temp_root() ->
    case os:getenv("TMPDIR") of
        Tmp when is_list(Tmp), Tmp =/= "" -> Tmp;
        _ -> "/tmp"
    end.

download(Url) ->
    {ok, _} = application:ensure_all_started([inets, ssl]),
    Ssl = [{verify, verify_peer},
           {cacerts, public_key:cacerts_get()},
           {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}],
    case httpc:request(get, {Url, []}, [{ssl, Ssl}, {autoredirect, true}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} -> {ok, Body};
        {ok, {{_, Status, _}, _Headers, _Body}} -> {error, {http_status, Status}};
        {error, Reason} -> {error, Reason}
    end.

sha256(Bin) ->
    binary_to_list(binary:encode_hex(crypto:hash(sha256, Bin), lowercase)).

run(Executable, Args) ->
    Port = open_port({spawn_executable, Executable}, [{args, Args}, exit_status, stderr_to_stdout]),
    wait_for(Port).

wait_for(Port) ->
    receive
        {Port, {data, Data}} ->
            io:format("~s", [Data]),
            wait_for(Port);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Status}} ->
            {error, {exit_status, Status}}
    end.

rebar3_config_path() ->
    filename:join([os:getenv("HOME"), ".config", "rebar3", "rebar.config"]).

consult_rebar3_config() ->
    case file:consult(rebar3_config_path()) of
        {ok, Terms} ->
            {ok, Terms};
        {error, enoent} ->
            %% File doesn't exist, we'll start with an empty config and create
            %% the file later.
            {ok, []};
        {error, Error} ->
            {error, Error}
    end.

has_nova_plugin(Plugins) ->
    lists:any(
      fun(rebar3_nova) -> true;
         (P) when is_tuple(P) -> element(1, P) =:= rebar3_nova;
         (_) -> false
      end, Plugins).

%% The global config is edited as text, never rewritten from parsed terms, so
%% comments and layout survive.
install_plugin(Terms) ->
    Path = rebar3_config_path(),
    case lists:keyfind(plugins, 1, Terms) of
        {plugins, Plugins} ->
            case has_nova_plugin(Plugins) of
                true -> already_installed;
                false -> insert_into_plugins(Path)
            end;
        false ->
            ok = filelib:ensure_dir(Path),
            Line = ["\n{plugins, [", ?NOVA_PLUGIN, "]}.\n"],
            ok = file:write_file(Path, Line, [append]),
            installed
    end.

insert_into_plugins(Path) ->
    {ok, Bin} = file:read_file(Path),
    case re:run(Bin, "^\\s*\\{\\s*plugins\\s*,\\s*\\[", [multiline]) of
        {match, [{Start, Len}]} ->
            <<Before:(Start + Len)/binary, After/binary>> = Bin,
            Separator = case {string:trim(After, leading), After} of
                            {<<"]", _/binary>>, _} -> "";
                            {_, <<"\n", _/binary>>} -> ",";
                            _ -> ", "
                        end,
            ok = file:write_file(Path, [Before, ?NOVA_PLUGIN, Separator, After]),
            installed;
        nomatch ->
            {add_by_hand, Path}
    end.

print_try_it() ->
    print(?TERM_GREEN, ?NOVA_LOGO),
    print(?TERM_GREEN, [""]),
    print(?TERM_GREEN, ["Congratulations, you have installed Nova plugin for rebar3!"]),
    print(?TERM_GREEN, ["Try it out with typing:"]),
    print(?TERM_BLUE, ["$ rebar3 new nova my_first_app"]).

main([]) ->
    %% Check for rebar3
    case check_for_rebar3() of
        ok ->
            %% Continue with the installation
            case consult_rebar3_config() of
                {ok, Rebar3Config} ->
                    case install_plugin(Rebar3Config) of
                        installed ->
                            print_try_it();
                        already_installed ->
                            print(?TERM_YELLOW, ["rebar3_nova is already listed in " ++ rebar3_config_path()]),
                            print_try_it();
                        {add_by_hand, Path} ->
                            print(?TERM_YELLOW, ["Your " ++ Path ++ " already has a plugins list.",
                                                 "Add " ++ ?NOVA_PLUGIN ++ " to it, then run:"]),
                            print(?TERM_BLUE, ["$ rebar3 new nova my_first_app"])
                    end;
                {error, _} ->
                    %% Failed to read rebar3 config file :/
                    print(?TERM_RED, ["Reading rebar3 configuration failed. Check your rebar.config file:"]),
                    print(?TERM_YELLOW, [rebar3_config_path()])
            end;
        _ ->
            print(?TERM_RED, ["Exiting installation"])
    end.
