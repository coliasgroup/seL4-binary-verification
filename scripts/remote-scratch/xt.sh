#!/usr/bin/env bash

set -eux -o pipefail

here=$(dirname $0)

# trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

# env="DISTRIBUTED_PROCESS_TRACE_CONSOLE=1 DISTRIBUTED_PROCESS_TRACE_FLAGS=pdnusrl"
env=""

# env $env cabal run remote-scratch -- worker &
# sleep 3
# env $env cabal run remote-scratch -- driver

env $env cabal run remote-scratch -- "$@"
