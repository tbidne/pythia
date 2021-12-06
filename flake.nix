{
  description = "A Haskell package for retrieving system information.";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.package-version-src = {
    url = "github:tbidne/package-version";
    inputs.flake-utils.follows = "flake-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.refined-simple-src = {
    url = "github:tbidne/refined-simple";
    inputs.flake-utils.follows = "flake-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , package-version-src
    , refined-simple-src
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc8107";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "pythia";
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
          overrides = final: prev: with pkgs.haskellPackages;
            let
              optics-core = callHackage "optics-core" "0.4" { };
              optics-th = callHackage "optics-th" "0.4"
                { inherit optics-core; };
              package-version =
                final.callCabal2nix "package-version" package-version-src { };
              refined-simple =
                final.callCabal2nix "refined-simple" refined-simple-src { };
            in
            {
              inherit
                optics-core
                optics-th
                package-version
                refined-simple;
            };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
