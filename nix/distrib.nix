{ lib
, buildEnv
, writeShellApplication

, yices
, bitwuzla

, package
}:

let
  workerSolvers = [
    yices
    bitwuzla
  ];

  driverSolvers = workerSolvers;

  worker = writeShellApplication {
    name = "worker";
    runtimeInputs = [ package ] ++ workerSolvers;
    text = ''
      exec sel4-bv-cli "$@"
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
    runtimeInputs = [ package run-worker-over-ssh ] ++ driverSolvers;
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
