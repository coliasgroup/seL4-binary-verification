let
  path =
    let
      rev = "4df1b885d76a54e1aa1a318f8d16fd6005b6401f"; # nixpkgs-unstable
    in
      builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        sha256 = "sha256:09225bj114bvg3j5v36a60gwq7hz8gcgy0nbfagq7b65gjwn25dy";
      };
in

# let
#   path = ../../../../x/nixpkgs;
# in

import path
