#!/usr/bin/env bash

export LANG="C.UTF-8"

cleanup () {
  echo "*** Cleaning test dir: $1 ***"
  cabal run profile-utils -- teardown "$1"
}

rts="-p"
ext="prof"

if [[ "$1" == "json" ]]; then
  rts="-pj"
  ext="json"
elif [[ -n "$1" ]]; then
  echo "Unrecognized arg: '$1'"
  exit 1
fi

echo "*** Running setup ***"

cmd_output=$(cabal run profile-utils -- setup)

rx="^.*test-dir: \|(.+)\|.*$"
if [[ $cmd_output =~ $rx ]]; then
    test_path="${BASH_REMATCH[1]}"
else
    echo "$cmd_output doesn't match" >&2
fi

echo "*** Test dir: $test_path ***"
trap "cleanup $test_path" EXIT

for i in "sync" "async" "pool"; do
  echo "*** Running $i on $test_path ***"
  cabal run profile --project-file cabal.profile.project -- +RTS $rts -RTS "$i" "$test_path"
  mv "profile.$ext" "profiling/app/profile_$i.$ext"
done
