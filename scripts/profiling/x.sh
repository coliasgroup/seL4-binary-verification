#!/usr/bin/env bash

set -eux -o pipefail

x=profiling-scratch

cabal build $x
path=$(cabal list-bin $x)

time $path
