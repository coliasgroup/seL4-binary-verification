#!/usr/bin/env bash

set -eux -o pipefail

x=par-scratch

cabal build $x
path=$(cabal list-bin $x)
# $path  +RTS -N1 -RTS "$@"
$path  +RTS -N40 -RTS "$@"
