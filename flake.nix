{
  description = "A utility for reporting path sizes.";
  inputs = {

    # nix
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";

    #haskell
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
      url = "github:tbidne/monad-effects/";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.smart-math.follows = "smart-math";
    };
    si-bytes = {
      url = "github:tbidne/si-bytes";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
  };
  outputs =
    inputs@{ flake-compat
    , flake-parts
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
                (nix-hs-utils.mkRelLib inputs.monad-effects final "effects-fs")
                (old: {
                  configureFlags = (old.configureFlags or [ ]) ++ [ "-f -os_path" ];
                });
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
              ormolu = prev.ormolu_0_5_3_0;
              unix-compat = prev.unix-compat_0_6;
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
              "si-bytes"
              "smart-math"
              "time-conv"
            ] // nix-hs-utils.mkRelLibs inputs.monad-effects final [
              "effects-async"
              "effects-exceptions"
              "effects-ioref"
              "effects-stm"
              "effects-thread"
              "effects-unix-compat"
            ];
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "path-size";
              root = ./.;
            };
          hsDirs = "app bench src test";
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
