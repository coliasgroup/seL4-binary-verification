#!/usr/bin/env bash

set -eux -o pipefail

here=$(dirname $0)

top=$here/../..
tmp=$top/tmp

solvers=$here/solvers.yaml
workers=$here/workers.yaml

sd=big
# sd=small
# sd=focused

d=$tmp/test-target-dirs/$sd

x=sel4-bv-cli

cabal build $x
path=$(cabal list-bin $x)

time $path \
    check \
    --solvers $solvers \
    --target-dir $d \
    --mismatch-dir $tmp/mismatch/local-check \
    --ignore-function fastpath_call \
    --ignore-function fastpath_reply_recv \
    --ignore-function-early c_handle_syscall \
    --ignore-function arm_swi_syscall \
    --rodata-section .rodata \
    --rodata-symbol kernel_device_frames \
    --rodata-symbol avail_p_regs \
    --file-log $here/../../tmp/logs/test-check.log.txt \
    --file-log-level debug \
    -j 16 \
    "$@"

    # --sqlite-cache $here/../tmp/cache.sqlite \
    # --log-level debug \
    # --include-function invokeTCB_WriteRegisters \
    # --include-group 42f7dc8a4b6f \
    # --workers $workers \
