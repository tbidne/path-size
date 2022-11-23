<div align="center">

# path-size

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/path-size?include_prereleases&sort=semver)](https://github.com/tbidne/path-size/releases/)
![haskell](https://img.shields.io/static/v1?label=&message=9.4&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![MIT](https://img.shields.io/github/license/tbidne/path-size?color=blue)](https://opensource.org/licenses/MIT)

[![nix](https://img.shields.io/github/workflow/status/tbidne/path-size/nix/main?label=nix&&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/path-size/actions/workflows/nix.yaml)
[![cabal](https://img.shields.io/github/workflow/status/tbidne/path-size/cabal/main?label=cabal&labelColor=2f353c)](https://github.com/tbidne/path-size/actions/workflows/cabal.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/path-size/stack/main?label=stack&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/path-size/actions/workflows/stack.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/path-size/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/path-size/actions/workflows/style.yaml)

</div>

---

### Table of Contents

- [Introduction](#introduction)
  - [Usage](#usage)
- [Options](#options)
  - [Number of paths](#number-of-paths)
  - [Exclude](#exclude)
  - [All](#all)
  - [Files only](#files-only)
  - [Depth](#depth)
  - [Strategy](#strategy)
- [Building](#building)
  - [Cabal](#cabal)
  - [Stack](#stack)
  - [Nix](#nix)

# Introduction

`path-size` is a CLI tool for calculating the (recursive) size of a given file-system path, and sorting subpaths by size. It is like the unix tool `du`, geared towards finding large files.

## Usage

```
path-size: A utility for reporting the recursive size of a directory.

Usage: path-size [-n|--num-paths (NAT | all)] [-e|--exclude PATHS...] [-a|--all]
                 [-f|--files-only] [-d|--depth NAT]
                 [-s|--strategy (async|sync|pool)] PATH [--version]

path-size allows one to find large paths on the file-system. In particular, the command will recursively associate a given path and all of its subpaths to their respective sizes.

Available options:
  -n,--num-paths (NAT | all)
                           The number of paths to display. If unspecified,
                           defaults to 10. The option 'all' returns everything.
  -e,--exclude PATHS...    Paths to skip. These must match the desired
                           directory/file name e.g. to skip /path/to/dir you
                           would pass '-e dir'. Note that this will exclude
                           _all_ subpaths that match 'dir'.
  -a,--all                 If enabled, searches hidden files/directories.
  -f,--files-only          If enabled, only sizes for files are calculated. All
                           directories are given size 0.
  -d,--depth NAT           The depth limit of our search. Note that we still
                           need to fully traverse the file system to get
                           accurate data; this argument merely affects what is
                           reported i.e. any depths > d are implicitly included
                           in parent directories, but not directly.
  -s,--strategy (async|sync|pool)
                           The search strategy is intended to improve
                           performance. The default is 'async', which uses
                           lightweight threads. The 'sync' option is a
                           sequential search and likely the slowest. Finally,
                           'pool' uses an explicit thread pool for concurrency.
                           This is potentially the fastest, though
                           experimentation is recommended.
  -h,--help                Show this help text

Version: 0.1
```

# Options

## Number of paths

**Arg:** `-n,--num-paths (NAT | all)`

**Description:** The number of paths to display. Defaults to 10. Can be a natural number of the string `all`, in which case all paths are returned.

**Examples:**

```
$ path-size -n 20 ./
...

$ path-size -n all ./
...
```

## Exclude

**Arg:** `-e,--exclude PATHS...`

**Description:** Paths to skip. These must match the desired directory/file name exactly e.g. to skip `/path/to/dir` you would pass `-e dir`. Note that this will exclude _all_ subpaths that match `dir`.

**Examples:**

```
$ path-size -e bar -e foo ./
```

## All

**Arg:** `-a, --all`

**Description:** If enabled, searches hidden files/directories.

**Examples:**

```
$ path-size -a ./
```

## Files only

**Arg:** `-f,--files-only`

**Description:** If enabled, only sizes for files are calculated. All directories are given size 0.

**Examples:**

```
$ path-size -f ./
```

## Depth

**Arg:** `-d,--depth NAT`

**Description:** The depth limit of our search. Note that we still need to fully traverse the file system to get accurate data; this argument merely affects what is reported i.e. any depths > d are implicitly included in parent directories, but not directly.

**Examples:**

```
$ path-size -d 1 ./
```

## Strategy

**Arg:** `-s,--strategy (async|sync|pool)`

**Description:**  The search strategy is intended to improve performance. The default is `async`, which uses lightweight threads. The `sync` option is a sequential search and likely the slowest. Finally, `pool` uses an explicit thread pool for concurrency. This is potentially the fastest, though experimentation is recommended.

**Examples:**

```
$ path-size -s pool ./
```

# Building

## Prerequisites

You will need one of:

* [cabal-install 2.4+](https://www.haskell.org/cabal/download.html) and one of
  * [ghc 9.2](https://www.haskell.org/ghcup/)
  * [ghc 9.4](https://www.haskell.org/ghcup/)
* [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
* [nix](https://nixos.org/download.html)

If you have never built a haskell program before, `cabal` + `ghcup` is probably the best choice.

## Cabal

You will need `ghc` and `cabal-install`. From there `path-size` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Stack

Like `cabal`, `path-size` can be built locally or installed globally (e.g. `~/.local/bin/`) with `stack build` and `stack install`, respectively.

## Nix

### From source

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `path-size` can be built with `nix build`, which will compile and run the tests.

To launch a shell with various tools (e.g. `cabal`, `hls`), run `nix develop`. After that we can launch a repl with `cabal repl` or run the various tools on our code. At this point you could also build via `cabal`, though you may have to first run `cabal update`. This will fetch the needed dependencies from `nixpkgs`.

### Via nix

Because `path-size` is a flake, it be built as part of a nix expression. For instance, if you want to add `path-size` to `NixOS`, your `flake.nix` might look something like:

```nix
{
  description = "My flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    path-size.url = "github:tbidne/path-size/main";
  };

  outputs = { self, nixpkgs, path-size, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        system = system;
      };
    in
    {
      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          system = system;
          modules = [
            (import ./configuration.nix { inherit pkgs path-size; })
          ];
        };
      };
    };
}
```

Then in `configuration.nix` you can simply have:

```nix
{ pkgs, path-size, ... }:

{
  environment.systemPackages = [
    path-size.packages.x86_64-linux.default
  ];
}
```