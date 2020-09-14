rebar3_nova
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_nova, {git, "https://host/user/rebar3_nova.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_nova
    ===> Fetching rebar3_nova
    ===> Compiling rebar3_nova
    <Plugin Output>
