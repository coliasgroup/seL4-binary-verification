#!/usr/bin/env bash

set -eux -o pipefail

here=$(dirname $0)

solvers=$here/solverlist.yaml

sd=big
# sd=small

d=tmp/test-target-dirs/$sd

x=cli

cabal build $x
path=$(cabal list-bin $x)

time $path \
    check \
    --solvers $solvers \
    --target-dir $d \
    --ignore-function fastpath_call \
    --ignore-function fastpath_reply_recv \
    --ignore-function c_handle_syscall \
    --ignore-function arm_swi_syscall \
    "$@"
