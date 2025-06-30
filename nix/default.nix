let
  nixpkgsPath =
    let
      rev = "a48741b083d4f36dd79abd9f760c84da6b4dc0e5"; # nixpkgs-unstable
    in
      builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        sha256 = "sha256:1in67kl5x9a0v3y0yw6fibpx1797k1d4s3nd2zfq5bwp7343ia84";
      };

  stacklock2nixPath =
    let
      rev = "108f6ab6d0208e3842443bea26dfd72b37d82f28";
    in
      builtins.fetchTarball {
        url = "https://github.com/cdepillabout/stacklock2nix/archive/${rev}.tar.gz";
        sha256 = "sha256:0vrhwqm6gwrqmjd4gm8ylwv1yvl0ngx7yx07wwxhwhmjfg32j26r";
      };
in

# let
#     nixpkgsPath = ../../../../x/nixpkgs;
#     stacklock2nixPath = ../../../../x/stacklock2nix;
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
