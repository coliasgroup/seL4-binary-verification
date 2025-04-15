let
  nixpkgsPath =
    let
      rev = "18dd725c29603f582cf1900e0d25f9f1063dbf11";
    in
      builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        sha256 = "sha256:0zrp7w41vqln7mmhvpb8ww6g6807bhic5c72mkqf9qh5336vc13b";
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

  shell = pkgs.callPackage ./shell.nix {
    inherit mathsat5 sonolar;
  };

  stacklock = pkgs.callPackage ./stacklock.nix {};

  stacklockPkgSet = stacklock.pkgSet;

  package = stacklockPkgSet.sel4-bv;

  mathsat5 = pkgs.callPackage ./solvers/mathsat5.nix {};
  sonolar = pkgs.callPackage ./solvers/sonolar.nix {};

  distrib = pkgs.callPackage ./distrib.nix {
    inherit
      package
    ;
  };
}
