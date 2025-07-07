#!/usr/bin/env bash

set -eu -o pipefail

here=$(dirname $0)

cd $here/../..

nix-shell --run "cabal run sel4-bv-cli -- $*"
