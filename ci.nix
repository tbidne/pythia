{ compiler ? "ghc8107"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/8a308775674e178495767df90c419425474582a1.tar.gz") { }
}:

let
  haskellDeps = ps: with ps; [
    cabal-install
    cabal-fmt
    hlint
  ];

  haskellOtherDeps = [ pkgs.haskellPackages.ormolu ];

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages haskellDeps;

  otherDeps = with pkgs; [
    nixpkgs-fmt
    zlib
  ];
in
pkgs.mkShell {
  buildInputs =
    [ ghc ]
    ++ haskellOtherDeps
    ++ otherDeps;
}
