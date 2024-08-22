#!/usr/bin/env bash

set -e

path_size_vers="0.1"

. /etc/lsb-release
ubuntu_vers=$(echo $DISTRIB_RELEASE)

arch=$(uname -m)

mkdir -p bin

# see NOTE: [Cabal Build vs. Install]
#
# Use cabal build for now for symmetry with linux static release.
cabal update
cabal build path-size:exe:path-size --ghc-options -Werror

cp ./dist-newstyle/build/x86_64-linux/ghc-*/path-size-*/x/path-size/opt/build/path-size/path-size "bin/path-size_$path_size_vers-$arch-linux-ubuntu_$ubuntu_vers"
