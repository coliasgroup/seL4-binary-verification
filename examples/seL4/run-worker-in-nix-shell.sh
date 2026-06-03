#!/usr/bin/env bash

set -eu -o pipefail

here=$(dirname $0)

toplevel=$here/../..

tmp=$here/tmp/local
mkdir -p $tmp

logfile=$tmp/log.$(date +%s).$RANDOM.txt

(cd $toplevel && nix-shell --run "\$(cabal list-bin sel4-bv-cli) $*") 2> >(tee $logfile >&2)
