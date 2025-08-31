# seL4 Binary Verification

This repository contains work towards a new implementation of seL4's [binary verification tool](https://github.com/seL4/graph-refine).

### Table of contents
- [Usage](#usage)
- [Example](#example)
- [Status](#status)

## Usage

```
$ sel4-bv-cli --help
seL4 Binary Verification

Usage: sel4-bv-cli COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  check                    Check proof scripts
  format-smt               Format SMT-LIB 2.0 S-expressions
  extract-smt              Extract solver input or output from a 'check' log
  _worker                  (internal) Spawn worker
```

### `sel4-bv-cli check`

This subcommand checks proof scripts discovered by the upstream tool.

```
seL4 Binary Verification

Usage: sel4-bv-cli check --solvers FILE [--workers FILE] 
                         [--online-solver-timeout SECONDS] 
                         [--offline-solver-timeout SECONDS] [--num-eval-cores N]
                         [--num-solver-cores N] [--sqlite-cache DATABASE] 
                         [--postgres-cache DATABASE]
                         --c-function-prefix C_FUNCTION_PREFIX 
                         [--rodata-section SECTION] [--rodata-symbol SYMBOL]
                         --target-dir DIRECTORY [--include-function SYMBOL] 
                         [--ignore-function SYMBOL] 
                         [--include-function-early SYMBOL] 
                         [--ignore-function-early SYMBOL] 
                         [--include-group FUNCTION:GROUP_FINGERPRINT] 
                         [--include-check FUNCTION:CHECK_FINGERPRINT] 
                         [--report-file FILE] [--dump-target-dir DIRECTORY] 
                         [--reference-target-dir DIRECTORY] 
                         [--mismatch-dir DIRECTORY] 
                         [--just-compare-to-reference] [--force-eval-all-stages]
                         [--log-level LEVEL] [--log-format FORMAT] 
                         [--file-log FILE [--file-log-level LEVEL] 
                           [--file-log-format FORMAT]]

  Check proof scripts

Available options:
  --solvers FILE           Solvers config file
  --workers FILE           Workers config file, for external workers mode
  --online-solver-timeout SECONDS
                           Timeout for online solver
  --offline-solver-timeout SECONDS
                           Timeout for offline solvers
  --num-eval-cores N       Number of cores to use for Haskell evaluation
  --num-solver-cores N     Number cores to use for SMT solvers, for embedded
                           worker mode
  --sqlite-cache DATABASE  SQLite database to use as a cache
  --postgres-cache DATABASE
                           PostgreSQL database to use as a cache
  --c-function-prefix C_FUNCTION_PREFIX
                           Prefix used for deriving C function name from ASM
                           function name
  --rodata-section SECTION Declare ELF section as rodata
  --rodata-symbol SYMBOL   Declare ELF symbol as rodata
  --target-dir DIRECTORY   Input target directory
  --include-function SYMBOL
                           Only check ASM functions specified by this option
  --ignore-function SYMBOL Don't check this ASM function
  --include-function-early SYMBOL
                           Only acknowledge ASM functions specified by this
                           option
  --ignore-function-early SYMBOL
                           Don't acknowledge this ASM function
  --include-group FUNCTION:GROUP_FINGERPRINT
                           Only check check groups specified by this option
  --include-check FUNCTION:CHECK_FINGERPRINT
                           Only check checks specified by this option
  --report-file FILE       Output file for report
  --dump-target-dir DIRECTORY
                           Dump stages into this directory
  --reference-target-dir DIRECTORY
                           Check stages against those found in this
  --mismatch-dir DIRECTORY Dump stage mismatches into this directory
  --just-compare-to-reference
                           Just compare stages to reference, skipping solver
                           invocations
  --force-eval-all-stages  Force evaluation of all stages
  --log-level LEVEL        Log level for stderr log
                           (error|warn|info|debug|trace)
  --log-format FORMAT      Log format for file log (human|text|json)
  --file-log FILE          Destination file for file log
  --file-log-level LEVEL   Log level for file log (error|warn|info|debug|trace)
  --file-log-format FORMAT Log format for file log (human|text|json)
  -h,--help                Show this help text
```

The directory specified using the required `--target-dir` option must contain the following files:

```
kernel.elf.rodata
kernel.elf.symtab
ASMFunctions.txt
CFunctions.txt
StackBounds.txt
inline-scripts.json
proof-scripts.json
```

The `handoff` branch of
[coliasgroup/graph-refine](https://github.com/coliasgroup/graph-refine/tree/handoff) has been
modified to emit `inline-scripts.json` and `proof-scripts.json` files. 

To facilitate the handoff phase of this project, both this tool and the upstream tool have been
equipped with features for ensuring that the two have identical behavior, down to the SMT-LIB 2.0
scripts that are sent to solvers. When supplied, this tool checks some of its intermediate stages
against those dumped by the upstream tool. When supplied, this tool finds those reference
intermediate stage dumps in the directory specified by the `--reference-dir` option.

The `handoff` branch of [coliasgroup/graph-refine](https://github.com/coliasgroup/graph-refine/tree/handoff) has been modified to emit these intermediate stage dump files, which include:


```
functions.txt
problems.txt
pairings.json
proof-checks.json
smt-proof-checks.json
```

The required `--solvers` option is for specifying a solver config YAML file. See
[./examples/seL4/solvers.yaml](./examples/seL4/solvers.yaml) for an example.

This tool has two modes: embedded worker mode and external worker mode. In embedded worker mode, the
process invoked on the command line runs the solvers itself, up to `--num-solver-cores` at a time.
In external worker mode, the process invoked on the command line spawns workers using the commands
in the workers config YAML file provided via the `--workers` option, and the workers run the
solvers. The tool uses the presence of the `--workers` option to determine whether to run in
external worker mode.

Each external worker command must result in a bidirectional stdio channel via which the command line
process can communicate with a process spawned from an identical binary from the command line
process itself. The `run-worker-over-ssh` script defined in [./nix/distrib.nix](./nix/distrib.nix)
copies the tool and its runtime dependencies (including solvers) to an SSH host and then runs it
over SSH. This script, or one like it, should cover most use cases. See
[./examples/seL4/workers.remote.yaml](./examples/seL4/workers.remote.yaml) for an example works
config file using this script.

A worker with `local: true` instead of a command is equivalent to `command: ["/proc/self/exe"]`.

## Example

See [./examples/seL4](./examples/seL4) for an example of how to use this tool to check an seL4 binary.

## Status

This tool can check proof scripts discovered by the upstream tool. The proof script search
is still in progress.
