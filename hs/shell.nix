let
  pkgs = import ./nixpkgs.nix {};

  ghcVersion = "96";

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

    hlint

    haskellPackages.ormolu
    haskellPackages.fourmolu
    haskellPackages.floskell
    haskellPackages.stylish-haskell

    haskellPackages.cabal-fmt

    haskellPackages.haskell-debug-adapter
    haskellPackages.haskell-dap
    haskellPackages.ghci-dap

    xdg-utils
  ];
}
