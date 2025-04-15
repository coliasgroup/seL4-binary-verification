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

  haskellPackagesOverlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = self': super': with self.haskell.lib.compose; {
        Cabal_3_14_1_0 = appendPatch (self.fetchpatch {
          # https://github.com/haskell/cabal/pull/10891
          url = "https://github.com/haskell/cabal/commit/c9411cbe729f1b432e30f860b40e4c3cc62c0e7e.patch";
          hash = "sha256-4nGD7+/U2l7DNqz6uf6PT4oFnrnbapEYEcH1TDnURWQ=";
          stripLen = 1;
        }) (doJailbreak super'.Cabal_3_14_1_0);
      };
    };
  };

  pkgs = nixpkgsFn {
    overlays = [
      stacklock2nixOverlay
      haskellPackagesOverlay
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
