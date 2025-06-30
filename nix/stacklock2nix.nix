let
  path =
    let
      rev = "108f6ab6d0208e3842443bea26dfd72b37d82f28";
    in
      builtins.fetchTarball {
        url = "https://github.com/cdepillabout/stacklock2nix/archive/${rev}.tar.gz";
        sha256 = "sha256:0vrhwqm6gwrqmjd4gm8ylwv1yvl0ngx7yx07wwxhwhmjfg32j26r";
      };
in

let
  path = ../../../../x/stacklock2nix;
in

import (path + "/nix/overlay.nix")
