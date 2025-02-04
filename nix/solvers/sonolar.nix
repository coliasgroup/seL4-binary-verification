{ lib, stdenv, fetchurl }:

stdenv.mkDerivation {
  name = "sonolar-binary";

  src = fetchurl {
    url = "https://www.informatik.uni-bremen.de/agbs/florian/sonolar/sonolar-2014-12-04-x86_64-linux.tar.gz";
    hash = "sha256-vzu03g07OVA95Db0OFx6i4BAYmrd6/9cIwtPT5Ka41g=";
  };

  phases = [ "unpackPhase" "installPhase" ];

  installPhase = ''
    cp -r . $out
  '';
}
