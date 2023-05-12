set -e

export LANG="C.UTF-8"

if [[ $1 == 'criterion' ]]; then
  cabal bench bench-criterion --benchmark-options \
    '--csv bench/criterion/bench.csv --output bench/criterion/bench.html'
else
  cabal bench bench-tasty --benchmark-options \
    '--csv bench/tasty/bench.csv --svg bench/tasty/bench.svg'
fi