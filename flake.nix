{
  description = "A Haskell package for retrieving system information.";
  inputs = {
    # nix
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";

    # haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
    };
    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.smart-math.follows = "smart-math";
    };
    si-bytes = {
      url = "github:tbidne/si-bytes";
      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
    };
    time-conv = {
      url = "github:tbidne/time-conv";
      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.monad-effects.follows = "monad-effects";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{ flake-parts
    , monad-effects
    , nix-hs-utils
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          ghc-version = "ghc962";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              file-io = final.callHackage "file-io" "0.1.0.1" { };
              hedgehog = prev.hedgehog_1_3;
              hlint = prev.hlint_3_6_1;
              ormolu = prev.ormolu_0_7_1_0;
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
              "si-bytes"
              "smart-math"
              "time-conv"
            ] // nix-hs-utils.mkRelLibs monad-effects final [
              "effects-env"
              "effects-exceptions"
              "effects-fs"
              "effects-ioref"
              "effects-optparse"
              "effects-stm"
              "effects-terminal"
              "effects-time"
              "effects-typed-process"
            ];
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "pythia";
              root = ./.;
            };
          hsDirs = "app exe-internal src test";
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format {
              inherit compiler hsDirs pkgs;
            };
            lint = nix-hs-utils.lint {
              inherit compiler hsDirs pkgs;
            };
            lint-refactor = nix-hs-utils.lint-refactor {
              inherit compiler hsDirs pkgs;
            };
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
