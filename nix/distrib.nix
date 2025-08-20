{ lib
, buildEnv
, writeShellApplication

, sel4-bv
, driverSolvers
, workerSolvers ? driverSolvers
}:

let
  worker = writeShellApplication {
    name = "worker";
    runtimeInputs = [ sel4-bv ] ++ workerSolvers;
    text = ''
      logfile=sel4-bv-worker-log.$(date +%s).$RANDOM.txt
      sel4-bv-cli "$@" 2> >(tee "$logfile" >&2)
    '';
  };

  run-worker-over-ssh = writeShellApplication {
    name = "run-worker-over-ssh";
    text = ''
      ssh_host="$1"
      shift

      ssh_opts=()
      while [[ $# -gt 0 ]]; do
        if [[ "$1" == "--" ]]; then
          shift
          break
        fi
        ssh_opts+=("$1")
        shift
      done

      args=()
      while [[ $# -gt 0 ]]; do
        args+=("$1")
        shift
      done

      NIX_SSHOPTS="''${ssh_opts[*]}" \
        nix-copy-closure \
          --use-substitutes \
          --gzip \
          --to "$ssh_host" \
          ${worker}

      exec ssh "$ssh_host" "''${ssh_opts[@]}" -- ${worker}/bin/${worker.name} "''${args[@]}"
    '';
  };

  driver = writeShellApplication {
    name = "driver";
    runtimeInputs = [ sel4-bv run-worker-over-ssh ] ++ driverSolvers;
    text = ''
      exec sel4-bv-cli "$@"
    '';
    passthru = {
      inherit
        worker
        run-worker-over-ssh
      ;
    };
  };

in
  driver
