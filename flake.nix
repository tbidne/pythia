{
  description = "A Haskell package for retrieving system information.";
  inputs = {
    # nix
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    byte-types = {
      url = "github:tbidne/byte-types";
      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    time-conv = {
      url = "github:tbidne/time-conv";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { algebra-simple
    , bounds
    , byte-types
    , flake-compat
    , flake-utils
    , nixpkgs
    , self
    , smart-math
    , time-conv
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      buildTools = c: with c; [
        cabal-install
        pkgs.gnumake
        pkgs.zlib
      ];
      devTools = c: with c; [
        ghcid
        haskell-language-server
      ];
      ghc-version = "ghc924";
      compiler = pkgs.haskell.packages."${ghc-version}";
      mkPkg = returnShellEnv: withDevTools:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "pythia";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv
              (buildTools compiler ++
                (if withDevTools then devTools compiler else [ ]));
          overrides = final: prev: with compiler; {
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple { };
            bounds =
              final.callCabal2nix "bounds" bounds { };
            byte-types =
              final.callCabal2nix "byte-types" byte-types { };
            smart-math =
              final.callCabal2nix "smart-math" smart-math { };
            time-conv =
              final.callCabal2nix "time-conv" time-conv { };
          };
        };
    in
    {
      packages.default = mkPkg false false;

      devShells.default = mkPkg true true;
      devShells.ci = mkPkg true false;
    });
}
