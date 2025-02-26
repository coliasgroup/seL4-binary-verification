#!/usr/bin/env bash

set -eux -o pipefail

here=$(dirname $0)

cd $here/..

nix-shell --run "cabal run cli -- worker $@"
# nix-shell --run "echo $@"
