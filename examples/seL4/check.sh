#!/usr/bin/env bash

set -eu -o pipefail

here=$(dirname $0)
tmp=$here/tmp

exe="cabal run sel4-bv-cli --"
# exe=$(nix-build -A distrib)/bin/driver

workers_arg="-j8"
# workers_arg="--workers $here/workers.local.yaml"
# workers_arg="--workers $here/workers.remote.yaml"

mkdir -p $tmp

$exe \
    check \
    --target-dir $here/target-dir \
    --rodata-section .rodata \
    --rodata-symbol kernel_device_frames \
    --rodata-symbol avail_p_regs \
    --ignore-function fastpath_call \
    --ignore-function fastpath_reply_recv \
    --ignore-function arm_swi_syscall \
    --ignore-function-early c_handle_syscall \
    $workers_arg \
    --solvers $here/solvers.yaml \
    --sqlite-cache $tmp/cache.sqlite \
    --file-log $tmp/log.txt \
    --file-log-level debug \
    "$@"

    # some other options:
    # --log-level debug \
    # --include-function invokeTCB_WriteRegisters \
    # --include-group 42f7dc8a4b6f \
