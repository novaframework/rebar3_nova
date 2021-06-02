#!/bin/bash

curl -o install.sh https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.escript
chmod +x install.escript
./install.escript
rm install.escript
