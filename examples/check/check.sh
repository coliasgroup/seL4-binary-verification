#!/usr/bin/env bash

set -eu -o pipefail

here=$(dirname $0)
top=$here/../..
tmp=$top/tmp

our_tmp=$here/tmp
mkdir -p $our_tmp

exe="cabal run sel4-bv-cli --"
# exe=$(nix-build -A distrib)/bin/driver

workers_arg="-j8"
# workers_arg="--workers $here/workers.local.yaml"
# workers_arg="--workers $here/workers.remote.yaml"

# size=big
size=small
# size=focused

target_dir=$tmp/test-target-dirs/$size

time $exe \
    check \
    --target-dir $target_dir \
    --ignore-function fastpath_call \
    --ignore-function fastpath_reply_recv \
    --ignore-function arm_swi_syscall \
    --ignore-function-early c_handle_syscall \
    --rodata-section .rodata \
    --rodata-symbol kernel_device_frames \
    --rodata-symbol avail_p_regs \
    $workers_arg \
    --solvers $here/solvers.yaml \
    --file-log $our_tmp/log.txt \
    --file-log-level debug \
    --sqlite-cache $our_tmp/cache.sqlite \
    "$@"

    # some other options:
    # --log-level debug \
    # --include-function invokeTCB_WriteRegisters \
    # --include-group 42f7dc8a4b6f \
