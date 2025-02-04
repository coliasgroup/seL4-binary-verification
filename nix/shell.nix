{ pkgs
, mathsat5
, sonolar
}:

let
  ghcVersion = "98";

  ghc = pkgs.haskell.compiler."ghc${ghcVersion}";

  hls = pkgs.haskell-language-server.override {
    supportedGhcVersions = [ ghcVersion ];
  };

in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    stack
    hls
    ghc
    cabal-install
    hpack

    hlint

    libz

    haskellPackages.ormolu
    haskellPackages.fourmolu
    haskellPackages.floskell
    haskellPackages.stylish-haskell

    haskellPackages.eventlog2html

    haskellPackages.haskell-debug-adapter
    haskellPackages.haskell-dap
    haskellPackages.ghci-dap

    inotify-tools

    yices
    bitwuzla
    z3
    cvc5
    cvc4
    mathsat5
    sonolar
  ];

  shellHook = ''
    mkdir -p $TMPDIR
  '';
}
