#!/usr/bin/env bash

set -e

export LANG="C.UTF-8"

for i in "sync" "async" "pool"; do
  echo "*** Running $i ***"
  cabal run profile --project-file cabal.profile.project -- +RTS -p -RTS
  mv profile.prof "profile/profile_$i.prof"
done
