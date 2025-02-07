#!/usr/bin/env bash

set -eux -o pipefail

x=par-scratch

cabal build $x
path=$(cabal list-bin $x)
$path "$@"
