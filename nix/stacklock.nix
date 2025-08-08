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
        call-stack = dontCheck hprev.call-stack;
        data-array-byte = dontCheck hprev.data-array-byte;
        distributed-process-simplelocalnet = dontCheck hprev.distributed-process-simplelocalnet;
        indexed-traversable-instances = dontCheck hprev.indexed-traversable-instances;
        integer-logarithms = dontCheck hprev.integer-logarithms;
        optics = dontCheck hprev.optics;
        optparse-applicative = dontCheck hprev.optparse-applicative;
        primitive = dontCheck hprev.primitive;
        scientific = dontCheck hprev.scientific;
        temporary = dontCheck hprev.temporary;
        time-compat = dontCheck hprev.time-compat;
        uuid-types = dontCheck hprev.uuid-types;
      };

  cabal2nixArgsOverrides = args: args // {
    "test-framework" = ver: {};
  };

  all-cabal-hashes =
    let
      rev = "3540f32d86870afc4cb76ccbab1e59164bc9e459";
    in
      fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
        sha256 = "sha256-bs42OV7aZhxLIpZKhUVtzFU5A1Wy7I6//70Y/ohpi8M=";
      };
}
