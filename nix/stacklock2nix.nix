let
  path =
    let
      rev = "27c7a46f9a4c73d3aef95d4a011213af0ce1e700";
    in
      builtins.fetchTarball {
        url = "https://github.com/cdepillabout/stacklock2nix/archive/${rev}.tar.gz";
        sha256 = "sha256:06p9bvgqchwwhb1a0bqh0j02zp05pg8ldx15vgiy4csls083ahb4";
      };
in

# let
#   path = ../../../../x/stacklock2nix;
# in

import (path + "/nix/overlay.nix")
