#!/usr/bin/env bash

curl -o install.escript https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.escript
chmod +x install.escript
./install.escript
rm install.escript
