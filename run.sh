#!/bin/sh
set -e

./build.sh

nix-shell --run "cabal run example-magic-card-search" "$@"
