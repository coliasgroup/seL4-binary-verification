{ lib
, fetchurl
, stacklock2nix
, haskell
}:

let
  localSrc = src: lib.cleanSourceWith {
    inherit src;
    filter = name: type:
      let
        root = src.origSrc or src;
        rel = lib.removePrefix "${toString root}/" (toString name);
      in
        lib.elem rel [
          "cabal.project"
          "cabal.project.freeze"
          "sel4-bv.cabal"
          "components"
        ] || lib.hasPrefix "components/" rel;
  };

in
stacklock2nix {
  stackYaml = ../stack.yaml;

  baseHaskellPkgSet = haskell.packages.ghc9102;

  additionalHaskellPkgSetOverrides =
    let
      inherit (haskell.lib.compose) dontCheck;
    in
      hfinal: hprev: {
        sel4-bv = (dontCheck hprev.sel4-bv).overrideAttrs (attrs: {
          src = localSrc (attrs.src);
        });
        # external
        distributed-process-simplelocalnet = dontCheck hprev.distributed-process-simplelocalnet;
        temporary = dontCheck hprev.temporary;
        call-stack = dontCheck hprev.call-stack;
        optparse-applicative = dontCheck hprev.optparse-applicative;
        optics = dontCheck hprev.optics;
      };

  cabal2nixArgsOverrides = args: args // {
    "test-framework" = ver: {};
  };

  all-cabal-hashes =
    let
      rev = "7e7c6724706b140016f949457fde4d26e629705c";
    in
      fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
        sha256 = "sha256-uVob2kuwWPDRLqLxKwfZhPGwQ8IcQhXiSp9PyDBmuZg=";
      };
}
