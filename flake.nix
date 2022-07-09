{
  description = "A Haskell package for retrieving system information.";
  inputs.algebra-simple-src.url = "github:tbidne/algebra-simple";
  inputs.byte-types-src.url = "github:tbidne/byte-types";
  inputs.env-guard-src.url = "github:tbidne/env-guard";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.smart-math-src.url = "github:tbidne/smart-math";
  outputs =
    { algebra-simple-src
    , byte-types-src
    , env-guard-src
    , flake-utils
    , nixpkgs
    , self
    , smart-math-src
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc923";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "pythia";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with compiler; [
              cabal-install
              haskell-language-server
              ghcid
              ormolu
              pkgs.nixpkgs-fmt
              pkgs.zlib
            ]);
          overrides = final: prev: with compiler; {
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple-src { };
            byte-types =
              final.callCabal2nix "byte-types" byte-types-src { };
            env-guard =
              final.callCabal2nix "env-guard" env-guard-src { };
            smart-math =
              final.callCabal2nix "smart-math" smart-math-src { };
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
