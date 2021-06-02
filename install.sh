#!/bin/bash

curl -o install.sh https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh
chmod +x install.sh
./install.sh
rm install.sh
