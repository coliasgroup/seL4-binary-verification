#!/usr/bin/env bash

set -eux -o pipefail

here=$(dirname $0)

solvers=$here/solvers.yaml
workers=$here/workers.yaml

sd=big
# sd=small
# sd=focused

d=tmp/test-target-dirs/$sd

x=distrib

path=$(nix-build -A $x --cores 12)/bin/driver

time $path \
    check \
    --solvers $solvers \
    --workers $workers \
    --target-dir $d \
    --ignore-function fastpath_call \
    --ignore-function fastpath_reply_recv \
    --ignore-function-early c_handle_syscall \
    --ignore-function arm_swi_syscall \
    --file-log $here/../logs/remote-test-check.log.txt \
    --file-log-level debug \
    -j 16 \
    "$@"

    # --sqlite-cache $here/../tmp/cache.sqlite \
    # --log-level debug \
    # --include-function invokeTCB_WriteRegisters \
    # --include-group 42f7dc8a4b6f \
