{
  description = "system-info flake";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.simple-algebra-src = {
    url = "github:tbidne/simple-algebra";
    inputs.flake-utils.follows = "flake-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.smart-data-src = {
    url = "github:tbidne/smart-data";
    inputs.flake-utils.follows = "flake-utils";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.simple-algebra-src.follows = "simple-algebra-src";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , simple-algebra-src
    , smart-data-src
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc8107";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "system-info";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
              cabal-fmt
              cabal-install
              cabal-plan
              haskell-language-server
              hlint
              ghcid
              implicit-hie
              ormolu
              pkgs.nixpkgs-fmt
              pkgs.zlib
            ]);
          overrides = final: prev: with pkgs.haskellPackages; {
            optics-core = callHackage "optics-core" "0.4" { };
            optics-th = callHackage "optics-th" "0.4" {
              optics-core = final.optics-core;
            };
            smart-data = final.callCabal2nix "smart-data" smart-data-src {
              simple-algebra =
                final.callCabal2nix "simple-algebra" simple-algebra-src { };
            };
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
