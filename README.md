# Rebar3 plugin for Nova
=====

A rebar plugin for nova

## Build

    $ rebar3 compile

## Basic usage

Add the plugin to your rebar config ( ~/.config/rebar3/rebar.config):

    {plugins, [
        {rebar3_nova, {git, "https://github.com/novaframework/rebar3_nova.git", {branch, "master"}}}
    ]}.

For latest stable from hex:
    
    {plugins, [rebar3_nova]}.

Then just call your plugin directly in an existing application:


    $ rebar3 nova
    ===> Fetching rebar3_nova
    ===> Compiling rebar3_nova
    <Plugin Output>

## Use auto-reload

Nova comes with a auto-reload mechanism that compiles and reloads code that is changed during runtime. This decreases the overall time when developing.

```
$ rebar3 nova serve
===> Verifying dependencies...
...
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.3  (abort with ^G)
1> ===> The rebar3 shell is a development tool; to deploy applications in production, consider using releases (http://www.rebar3.org/docs/releases)
...
```
