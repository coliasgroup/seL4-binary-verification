let
  nixpkgsFn = import ./nixpkgs.nix;

  stacklock2nixOverlay = import ./stacklock2nix.nix;

  pkgs = nixpkgsFn {
    overlays = [
      stacklock2nixOverlay
    ];
  };

in rec {
  inherit pkgs;

  mathsat5 = pkgs.callPackage ./solvers/mathsat5.nix {};
  sonolar = pkgs.callPackage ./solvers/sonolar.nix {};

  shell = pkgs.callPackage ./shell.nix {
    inherit mathsat5 sonolar;
  };

  stacklock = pkgs.callPackage ./stacklock.nix {};

  inherit (stacklock.pkgSet) sel4-bv;

  distrib = pkgs.callPackage ./distrib.nix {
    inherit sel4-bv;
    driverSolvers = with pkgs; [
      yices
      bitwuzla
    ];
  };
}
