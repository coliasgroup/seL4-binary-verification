{ lib, stdenv
, fetchurl
, autoPatchelfHook
, gmp
}:

stdenv.mkDerivation {
  name = "mathsat5-binary";
  src = fetchurl {
    url = "https://mathsat.fbk.eu/download.php?file=mathsat-5.6.10-linux-x86_64.tar.gz";
    hash = "sha256-7/M9bMx2FDz8oJ9pHd5JGjnowW832iC5yq4407wzfuY=";
  };
  phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
  nativeBuildInputs = [
    autoPatchelfHook
  ];
  buildInputs = [
    gmp
  ];
  installPhase = ''
    cp -r . $out
  '';
}
