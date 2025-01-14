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

  baseHaskellPkgSet = haskell.packages.ghc966;

  additionalHaskellPkgSetOverrides =
    let
      inherit (haskell.lib.compose) dontCheck;
    in
      hfinal: hprev: {
        sel4-bv = (dontCheck hprev.sel4-bv).overrideAttrs (attrs: {
          src = localSrc (attrs.src);
        });
        # external
        prettyprinter = dontCheck hprev.prettyprinter;
        lifted-base = dontCheck hprev.lifted-base;
      };

  all-cabal-hashes =
    let
      rev = "e55dd9bb019f3069139d7f2d7d035858f95d2ec8";
    in
      fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
        sha256 = "sha256-W0GgaY70OQk1vgGcKUFfVZq8Mn0NnKl/JQEi2EBAfdQ=";
      };
}
