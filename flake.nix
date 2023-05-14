{
  description = "A Haskell package for retrieving system information.";
  inputs = {
    # nix
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-hs-utils = {
      url = "github:tbidne/nix-hs-utils";
      inputs.flake-compat.follows = "flake-compat";
    };

    # haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.smart-math.follows = "smart-math";
    };
    si-bytes = {
      url = "github:tbidne/si-bytes";
      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    time-conv = {
      url = "github:tbidne/time-conv";
      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.monad-effects.follows = "monad-effects";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{ flake-compat
    , flake-parts
    , monad-effects
    , nix-hs-utils
    , nixpkgs
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          ghc-version = "ghc944";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              apply-refact = prev.apply-refact_0_11_0_0;
              effects-fs = hlib.overrideCabal
                (nix-hs-utils.mkRelLib monad-effects final "effects-fs")
                (old: {
                  configureFlags = (old.configureFlags or [ ]) ++ [ "-f -os_path" ];
                });
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
              ormolu = prev.ormolu_0_5_3_0;
              tasty-hedgehog = prev.tasty-hedgehog_1_4_0_0;
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
              "si-bytes"
              "smart-math"
              "time-conv"
            ] // nix-hs-utils.mkRelLibs monad-effects final [
              "effects-env"
              "effects-exceptions"
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
          hs-dirs = "app exe-internal src test";
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format {
              inherit compiler hs-dirs pkgs;
            };
            lint = nix-hs-utils.lint {
              inherit compiler hs-dirs pkgs;
            };
            lint-refactor = nix-hs-utils.lint-refactor {
              inherit compiler hs-dirs pkgs;
            };
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
