{
  description = "A Haskell package for retrieving system information.";
  inputs.algebra-simple-src.url = "github:tbidne/algebra-simple?rev=1b2bd9fd21ecf5ed6635cc5c14d06ab533950df3";
  inputs.byte-types-src.url = "github:tbidne/byte-types?rev=9f2894613a4ac8f4f704029e6b34073b1f2c88a8";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs?rev=1ffba9f2f683063c2b14c9f4d12c55ad5f4ed887";
  inputs.smart-math-src.url = "github:tbidne/smart-math?rev=35e508b0e4272e32ba2841dccbcbb8294d4d546c";
  outputs =
    { algebra-simple-src
    , byte-types-src
    , flake-utils
    , nixpkgs
    , self
    , smart-math-src
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc922";
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
