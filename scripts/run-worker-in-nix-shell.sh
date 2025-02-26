#!/usr/bin/env bash

set -eux -o pipefail

here=$(dirname $0)

cd $here/..

nix-shell --run "cabal run sel4-bv-cli -- $*"
# nix-shell --run "echo $@"
