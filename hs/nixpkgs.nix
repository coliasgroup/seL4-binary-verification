let
  nixpkgsPath =
    let
      rev = "a27871180d30ebee8aa6b11bf7fef8a52f024733";
    in
      builtins.fetchTarball {
        url = "https://github.com/coliasgroup/nixpkgs/archive/${rev}.tar.gz";
        sha256 = "sha256:0b4lrpaiq1h8frzjyz2bp45r6xv5fh956lqn9li04a781cafx0a3";
      };
in

# let
#   nixpkgsPath = ../../../nixpkgs;
# in

import nixpkgsPath
