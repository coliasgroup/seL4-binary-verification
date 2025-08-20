#!/usr/bin/env bash

set -eu -o pipefail

here=$(dirname $0)
toplevel=$here/../..
tmp=$here/tmp

exe="cabal run sel4-bv-cli --"

# Required for workers.remote.yaml (see below)
# exe=$(nix-build $toplevel -A distrib)/bin/driver

# Three options for $worker_args:

# Embedded worker
workers_arg="--num-solver-cores 8"

# External workers via SSH on localhost
# workers_arg="--workers $here/workers.local.yaml"

# External workers via SSH on remote hosts (requires Nix-built $exe, see above)
# workers_arg="--workers $here/workers.remote.yaml"

mkdir -p $tmp

$exe \
    check \
    $workers_arg \
    --solvers $here/solvers.yaml \
    --num-eval-cores 8 \
    --sqlite-cache $tmp/cache.sqlite \
    --c-function-prefix Kernel_C. \
    --rodata-section .rodata \
    --rodata-symbol kernel_device_frames \
    --rodata-symbol avail_p_regs \
    --ignore-function fastpath_call \
    --ignore-function fastpath_reply_recv \
    --ignore-function arm_swi_syscall \
    --ignore-function-early c_handle_syscall \
    --target-dir $here/target-dir \
    --file-log $tmp/log.txt \
    --file-log-level debug \
    "$@"
