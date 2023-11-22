<div align="center">

# path-size

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/path-size?include_prereleases&sort=semver)](https://github.com/tbidne/path-size/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/path-size/ci.yaml?branch=main)](https://github.com/tbidne/path-size/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/path-size?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

### Table of Contents

- [Introduction](#introduction)
  - [Usage](#usage)
- [Options](#options)
  - [All](#all)
  - [Depth](#depth)
  - [Exclude](#exclude)
  - [Files only](#files-only)
  - [Ignore dir size](#ignore-dir-size)
  - [Number of paths](#number-of-paths)
  - [Reverse](#reverse)
  - [Stable](#stable)
  - [Strategy](#strategy)
- [Building](#building)
  - [Cabal](#cabal)
  - [Nix](#nix)

# Introduction

`path-size` is a CLI tool for calculating the (recursive) size of a given file-system path, and sorting subpaths by size. It is like the unix tool `du`, geared towards finding large files.

## Usage

```
path-size: A utility for reporting the recursive size of a directory.

Usage: path-size [-a|--all] [-d|--depth NAT] [-e|--exclude PATHS...]
                 [-f|--files-only] [--ignore-dir-size]
                 [-n|--num-paths (NAT | all)] [-r|--reverse] [--stable]
                 [-s|--strategy (async|sync|pool)] [-v|--version] PATH

  path-size allows one to find large paths on the file-system. In particular,
  the command will recursively associate a given path and all of its subpaths to
  their respective sizes.

Available options:
  -a,--all                 If enabled, searches hidden files/directories.

  -d,--depth NAT           The depth limit of our search. Note that we still
                           need to fully traverse the file system to get
                           accurate data; this argument merely affects what is
                           reported i.e. any depths > d are implicitly included
                           in parent directories, but not directly.

  -e,--exclude PATHS...    Paths to skip. These must match the desired
                           directory/file name e.g. to skip /path/to/dir you
                           would pass '-e dir'. Note that this will exclude
                           _all_ subpaths that match 'dir'.

  -f,--files-only          If enabled, only sizes for files are calculated. All
                           directories are given size 0. Note this effectively
                           implies --ignore-dir-size.

  --ignore-dir-size        If enabled, ignores the size of the directories
                           themselves i.e. a directory's size is determined by
                           the sum of all of its subfiles, only. The size of the
                           directory itself (e.g. 4096 bytes on a typical ext4
                           filesystem) is ignored.

  -n,--num-paths (NAT | all)
                           The number of paths to display. If unspecified,
                           defaults to 10. The option 'all' returns everything.

  -r,--reverse             If enabled, paths are sorted in reverse (ascending)
                           order.

  --stable                 If enabled, an additional sorting filter is applied
                           to sort by path name. This allows the sorted order to
                           be deterministic (as paths are unique), at the cost
                           of performance.

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

## All

**Arg:** `-a, --all`

**Description:** If enabled, searches hidden files/directories.

**Examples:**

```
$ path-size -a ./
```

## Depth

**Arg:** `-d,--depth NAT`

**Description:** The depth limit of our search. Note that we still need to fully traverse the file system to get accurate data; this argument merely affects what is reported i.e. any depths > d are implicitly included in parent directories, but not directly.

**Examples:**

```
$ path-size -d 1 ./
```

## Exclude

**Arg:** `-e,--exclude PATHS...`

**Description:** Paths to skip. These must match the desired directory/file name exactly e.g. to skip `/path/to/dir` you would pass `-e dir`. Note that this will exclude _all_ subpaths that match `dir`.

**Examples:**

```
$ path-size -e bar -e foo ./
```

## Files only

**Arg:** `-f,--files-only`

**Description:** If enabled, only sizes for files are calculated. All directories are given size 0. Note this effectively implies `--ignore-dir-size`.

**Examples:**

```
$ path-size -f ./
```

## Ignore dir size

**Arg:** `--ignore-dir-size`

**Description:** If enabled, ignores the size of the directories themselves i.e. a directory's size is determined by the sum of all of its subfiles, only. The size of the directory itself (e.g. 4096 bytes on a typical ext4 filesystem) is ignored.

**Examples:**

```
$ path-size --ignore-dir-size ./
```

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

## Reverse

**Arg:** `-r,--reverse`

**Description:** If enabled, paths are sorted in reverse (ascending) order.

**Examples:**

```
$ path-size -r ./
```

## Stable

**Arg:** `--stable`

**Description:** If enabled, an additional sorting filter is applied to sort by path name. This allows the sorted order to be deterministic (as paths are unique), at the cost of performance.

**Examples:**

```
$ path-size --stable ./
```

## Strategy

**Arg:** `-s,--strategy (async|sync|pool)`

**Description:** The search strategy is intended to improve performance. The default is `async`, which uses lightweight threads. The `sync` option is a sequential search and likely the slowest. Finally, `pool` uses an explicit thread pool for concurrency. This is potentially the fastest, though experimentation is recommended.

**Examples:**

```
$ path-size -s pool ./
```

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`ghcup`](https://www.haskell.org/ghcup/)

Using `ghcup`, install `cabal 2.4+` and one of:

- `ghc 9.2`
- `ghc 9.4`
- `ghc 9.6`

### Build path-size

Once you have `cabal` and `ghc`, `path-size` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Nix

### Prerequisites

* [nix](https://nixos.org/download.html)

### Manually

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `path-size` can be built with `nix build`, which will compile and run the tests.

### Nix expression

Because `path-size` is a flake, it be built as part of a nix expression. For instance, if you want to add `path-size` to `NixOS`, your `flake.nix` should have:

```nix
# flake.nix
{
  inputs.path-size.url = "github:tbidne/path-size/main";
}
```

Then include this in the `systemPackages`:

```nix
# wherever your global packages are defined
{
  environment.systemPackages = [
    path-size.packages."${system}".default
  ];
}
```