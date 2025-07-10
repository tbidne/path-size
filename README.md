<div align="center">

# path-size

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/path-size?include_prereleases&sort=semver)](https://github.com/tbidne/path-size/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/path-size/ci.yaml?branch=main)](https://github.com/tbidne/path-size/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/path-size?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

### Table of Contents

- [Introduction](#introduction)
- [Options](#options)
  - [All](#all)
  - [Depth](#depth)
  - [Exclude](#exclude)
  - [Files only](#files-only)
  - [Format](#format)
  - [Ignore dir size](#ignore-dir-size)
  - [No color](#no-color)
  - [Number of paths](#number-of-paths)
  - [Reverse](#reverse)
  - [Stable](#stable)
  - [Strategy](#strategy)
- [Building](#building)
  - [Cabal](#cabal)
  - [Nix](#nix)

# Introduction

`path-size` is a CLI tool for calculating the (recursive) size of a given file-system path, and sorting subpaths by size. It is like the unix tool `du`, geared towards finding large files.

```
$ path-size ./src
Size   | Dirs | Files | Path
-------------------------------------------------------------------
62.37K | 4    | 9     | ./src
48.29K | 3    | 8     | ./src/PathSize
39.43K | 2    | 6     | ./src/PathSize/Data
16.11K | 1    | 1     | ./src/PathSize/Data/SubPathData
12.01K | 0    | 1     | ./src/PathSize/Data/SubPathData/Internal.hs
9.99K  | 0    | 1     | ./src/PathSize.hs
9.68K  | 0    | 1     | ./src/PathSize/Data/Config.hs
4.15K  | 0    | 1     | ./src/PathSize/Data/PathData.hs
3.77K  | 0    | 1     | ./src/PathSize/Utils.hs
2.76K  | 0    | 1     | ./src/PathSize/Data/PathSizeResult.hs
```

# Options

## All

**Arg:** `-a, --all`

**Description:** Searches hidden files/directories. We only consider hidden files per the unix dot convention (e.g. `.hidden_path`). All files are considered unhidden on windows.

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

**Description:** Only sizes for files are calculated. All directories are given size 0. Note this effectively implies `--ignore-dir-size`.

**Examples:**

```
$ path-size -f ./
```

## Format

**Arg:** `--format`

**Description:** Formatting options.

  - `(s|single)`: Simply, single-line format.
  - `(t|tabular)`: The default. Prints a table.

**Examples:**

```
$ path-size --format tabular test/functional/data/success
Size   | Dirs | Files | Path
----------------------------------------------------------
24.60K | 6    | 4     | test/functional/data/success
16.40K | 4    | 2     | test/functional/data/success/d2
8.21K  | 2    | 1     | test/functional/data/success/d2/d2

$ path-size --format single test/functional/data/success
test/functional/data/success: 24.60K, Directories: 6, Files: 4
test/functional/data/success/d2: 16.40K, Directories: 4, Files: 2
test/functional/data/success/d2/d2: 8.21K, Directories: 2, Files: 1
```

## Ignore dir size

**Arg:** `--ignore-dir-size`

**Description:** Ignores the size of the directories themselves i.e. a directory's size is determined by the sum of all of its subfiles, only. The size of the directory itself (e.g. 4096 bytes on a typical ext4 filesystem) is ignored.

**Examples:**

```
$ path-size --ignore-dir-size ./
```

## No color

**Arg:** `--no-color`

**Description:** Disables output colors.

## Number of paths

**Arg:** `-n,--num-paths (NAT | all)`

**Description:** The number of paths to display. If unspecified, defaults to 10. The option 'all' returns everything.

**Examples:**

```
$ path-size -n 20 ./
...

$ path-size -n all ./
...
```

## Reverse

**Arg:** `-r,--reverse`

**Description:** Paths are sorted in reverse (ascending) order.

**Examples:**

```
$ path-size -r ./
```

## Stable

**Arg:** `--stable`

**Description:** An additional sorting filter is applied to sort by path name. This allows the sorted order to be deterministic (as paths are unique), at the cost of performance.

**Examples:**

```
$ path-size --stable ./
```

## Strategy

**Arg:** `-s,--strategy (async | sync | pool)`

**Description:** The search strategy is intended to improve performance. The default is `async`, which uses lightweight threads. The `sync` option is a sequential search and likely the slowest. Finally, `pool` uses an explicit thread pool for concurrency. This is potentially the fastest, though experimentation is recommended.

**Examples:**

```
$ path-size -s pool ./
```

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`cabal 2.4+`](https://www.haskell.org/cabal/download.html)
* [`ghc 9.2 - 9.12`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)

The easiest way to install these is generally [`ghcup`](https://www.haskell.org/ghcup/).

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
