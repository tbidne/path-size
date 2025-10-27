{
  description = "A utility for reporting path sizes.";
  inputs = {

    # nix
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";

    #haskell
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
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
    };
    fs-utils = {
      url = "github:tbidne/fs-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
      inputs.smart-math.follows = "smart-math";
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
    unicode-grapheme = {
      url = "github:tbidne/unicode-grapheme";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
    };
  };
  outputs =
    inputs@{
      flake-parts,
      nix-hs-utils,
      nixpkgs,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          ghc-version = "ghc9122";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                Cabal-syntax_3_10_3_0 = hlib.doJailbreak prev.Cabal-syntax_3_10_3_0;

                # TODO: Remove optparse override and jailbreaks once former
                # is the default.
                fourmolu = hlib.doJailbreak prev.fourmolu;
                hspec-golden = hlib.doJailbreak prev.hspec-golden;
                ormolu = hlib.doJailbreak prev.ormolu;
                optparse-applicative = prev.optparse-applicative_0_19_0_0;
                stylish-haskell = hlib.doJailbreak prev.stylish-haskell;
                 
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
                "si-bytes"
                "smart-math"
              ]
              // nix-hs-utils.mkRelLibs "${inputs.monad-effects}/lib" final [
                "effects-async"
                "effects-fs"
                "effects-ioref"
                "effects-optparse"
                "effects-stm"
                "effects-thread"
                "effects-unix"
                "effects-unix-compat"
              ]
              // nix-hs-utils.mkRelLibs "${inputs.unicode-grapheme}/lib" final [
                "unicode-grapheme"
              ];
          };
          hlib = pkgs.haskell.lib;
          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "path-size";
              root = ./.;

              modifier =
                drv:
                drv.overrideAttrs (oldAttrs: {
                  PATH_SIZE_HASH = "${self.rev or self.dirtyRev}";
                  PATH_SIZE_MODIFIED = "${builtins.toString self.lastModified}";
                  PATH_SIZE_SHORT_HASH = "${self.shortRev or self.dirtyShortRev}";

                  # Git is needed to run the tests (git diff).
                  nativeBuildInputs = oldAttrs.nativeBuildInputs or [ ] ++ [
                    pkgs.git
                  ];
                });
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
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
