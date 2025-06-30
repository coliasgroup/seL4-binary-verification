let
  path =
    let
      rev = "a48741b083d4f36dd79abd9f760c84da6b4dc0e5"; # nixpkgs-unstable
    in
      builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        sha256 = "sha256:1in67kl5x9a0v3y0yw6fibpx1797k1d4s3nd2zfq5bwp7343ia84";
      };
in

# let
#   path = ../../../../x/nixpkgs;
# in

import path
