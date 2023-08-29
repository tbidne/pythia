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
    effectful-effects = {
      url = "github:tbidne/effectful-effects";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
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
      url = "github:tbidne/time-conv/effectful";
      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.effectful-effects.follows = "effectful-effects";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{ flake-parts
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
              typed-process-effectful =
                hlib.dontCheck
                (final.callHackageDirect
                  {
                    pkg = "typed-process-effectful";
                    ver = "1.0.0.0";
                    sha256 = "sha256-+AGzviNpE6sIf8j8IQ6qjEjIILe82mItZSEkc/Qc34c=";
                  }
                  {});
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
              "si-bytes"
              "smart-math"
              "time-conv"
            ] // nix-hs-utils.mkRelLibs "${inputs.effectful-effects}/lib" final [
              "env-effectful"
              "exceptions-effectful"
              "fs-effectful"
              "ioref-effectful"
              "optparse-effectful"
              "terminal-effectful"
              "time-effectful"
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
