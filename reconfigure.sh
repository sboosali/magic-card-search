#!/bin/bash
set -e

cabal2nix . > default.nix

./build.sh
