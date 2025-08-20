# seL4 Example

The [./check.sh](./check.sh) script demonstrates the use of this tool in checking an seL4 binary.

Note the comments about the different options for `$worker_arg` and `$exe`. The embedded worker
configuration (default) requires the solvers in [./solvers.yaml](./solvers.yaml) ([Yices
2](https://yices.csl.sri.com/) and [Bitwuzla](https://bitwuzla.github.io/)) to be present in
`$PATH`. Both the `workers.local.yaml` and `workers.remote.yaml` configurations require the [Nix
package manager](https://nixos.org/) to be installed.

See [../../HACKING.md](../../HACKING.md) for information about the environment required to build
this tool.
