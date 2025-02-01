let
  nixpkgsPath =
    let
      rev = "9189ac18287c599860e878e905da550aa6dec1cd";
    in
      builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        sha256 = "sha256:0hbbpljkrj2mn5giq8cympf3cn6ihcrdm2n4glsdbwgd3hadm2q1";
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

  package = stacklockPkgSet.sel4-bv;
}
