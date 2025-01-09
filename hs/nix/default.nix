let
  nixpkgsPath =
    let
      rev = "a27871180d30ebee8aa6b11bf7fef8a52f024733";
    in
      builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        sha256 = "sha256:0b4lrpaiq1h8frzjyz2bp45r6xv5fh956lqn9li04a781cafx0a3";
      };

  stacklock2nixPath =
    let
      rev = "3cb77e9c869be90ad939f368096ff2cc940881d4";
    in
      builtins.fetchTarball {
        url = "https://github.com/cdepillabout/stacklock2nix/archive/${rev}.tar.gz";
        sha256 = "sha256:09mcf8w4wdvzp0k4wk9ksv0ksfnnlp216ccqvqy1ijlv58gp9pbm";
      };
in

# let
#   nixpkgsPath = ../../../nixpkgs;
# in

let
  nixpkgsFn = import nixpkgsPath;

  stacklock2nixOverlay = import (stacklock2nixPath + "/nix/overlay.nix");

  pkgs = nixpkgsFn {
    overlays = [
      stacklock2nixOverlay
    ];
  };

in rec {
  inherit nixpkgsFn;

  inherit pkgs;

  shell = pkgs.callPackage ./shell.nix {};

  stacklock = pkgs.callPackage ./stacklock.nix {};

  stacklockPkgSet = stacklock.pkgSet;

  package = stacklockPkgSet.bv;
}
