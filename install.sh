#!/usr/bin/env bash
set -euo pipefail

# The installer is taken from a release tag, never from a moving branch.
REF="${REBAR3_NOVA_REF:-v0.14.0}"

DIR="$(mktemp -d)"
trap 'rm -rf "$DIR"' EXIT

curl -fsSL -o "$DIR/install.escript" \
    "https://raw.githubusercontent.com/novaframework/rebar3_nova/$REF/install.escript"
escript "$DIR/install.escript"
