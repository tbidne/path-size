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
    byte-types = {
      url = "github:tbidne/byte-types";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
  };
  outputs =
    { algebra-simple
    , bounds
    , byte-types
    , flake-compat
    , flake-parts
    , monad-effects
    , nixpkgs
    , self
    }:
    flake-parts.lib.mkFlake { inherit self; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: with c; [
            cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          # add tools like hlint, ormolu, ghcid here if you want them
          # on the PATH
          devTools = c: with c; [
            ghcid
            haskell-language-server
          ];
          ghc-version = "ghc925";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            };
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv: withDevTools:
            compiler.developPackage {
              inherit returnShellEnv;
              name = "path-size";
              root = ./.;
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (buildTools compiler ++
                    (if withDevTools then devTools compiler else [ ]));
              overrides = final: prev: with compiler; {
                algebra-simple =
                  final.callCabal2nix "algebra-simple" algebra-simple { };
                bounds = final.callCabal2nix "bounds" bounds { };
                byte-types = final.callCabal2nix "byte-types" byte-types { };
                monad-callstack =
                  final.callCabal2nix "monad-callstack"
                    "${monad-effects}/monad-callstack"
                    { };
                monad-fs =
                  final.callCabal2nix "monad-fs"
                    "${monad-effects}/monad-fs"
                    { };
              };
            };
        in
        {
          packages.default = mkPkg false false;
          devShells.default = mkPkg true true;
          devShells.ci = mkPkg true false;
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
