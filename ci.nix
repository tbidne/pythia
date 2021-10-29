{ compiler ? "ghc8107"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/a4bf44345706231f9dd56f85757499af1e940847.tar.gz") { }
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
