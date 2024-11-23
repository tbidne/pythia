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
    exception-utils = {
      url = "github:tbidne/exception-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fs-utils = {
      url = "github:tbidne/fs-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    effectful-libs = {
      url = "github:tbidne/effectful-libs";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
    };
    si-bytes = {
      url = "github:tbidne/si-bytes";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    smart-math = {
      url = "github:tbidne/smart-math";

      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    time-conv = {
      url = "github:tbidne/time-conv/effectful";

      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
      inputs.effectful-libs.follows = "effectful-libs";
    };
  };
  outputs =
    inputs@{
      flake-parts,
      effectful-libs,
      nix-hs-utils,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          ghc-version = "ghc982";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                effectful-core = (
                  final.callHackageDirect {
                    pkg = "effectful-core";
                    ver = "2.5.0.0";
                    sha256 = "sha256-UCbMP8BfNfdIRTLzB4nBr17jxRp5Qmw3sTuORO06Npg=";
                  } { }
                );
                effectful = (
                  final.callHackageDirect {
                    pkg = "effectful";
                    ver = "2.5.0.0";
                    sha256 = "sha256-lmM0kdM5PS45Jol5Y2Nw30VWWfDPiPJLrwVj+GmJSOQ=";
                  } { }
                );
                strict-mutable-base = (
                  final.callHackageDirect {
                    pkg = "strict-mutable-base";
                    ver = "1.1.0.0";
                    sha256 = "sha256-cBSwoNGU/GZDW3eg7GI28t0HrrrxMW9hRapoOL2zU7Q=";
                  } { }
                );
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
                "si-bytes"
                "smart-math"
                "time-conv"
              ]
              // nix-hs-utils.mkRelLibs "${effectful-libs}/lib" final [
                "environment-effectful"
                "fs-effectful"
                "ioref-effectful"
                "optparse-effectful"
                "stm-effectful"
                "terminal-effectful"
                "time-effectful"
                "typed-process-dynamic-effectful"
                "unix-compat-effectful"
              ];
          };
          hlib = pkgs.haskell.lib;
          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "pythia";
              root = ./.;
            };
          compilerPkgs = {
            inherit compiler pkgs;
          };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            lint = nix-hs-utils.lint compilerPkgs;
            lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
        "x86_64-linux"
      ];
    };
}
