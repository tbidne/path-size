set -e

export LANG="C.UTF-8"

criterion=0
tasty=1

if [[ $1 == 'criterion' ]]; then
  criterion=1
  tasty=0
elif [[ $1 == 'tasty' ]]; then
  criterion=0
  tasty=1
elif [[ $1 == 'all' ]]; then
  criterion=1
  tasty=1
else
  echo "Unrecognized arg. Wanted one of [all|criterion|tasty]. received: '$1'"
  exit 1
fi

if [[ 1 == $criterion ]]; then
  cabal bench bench-criterion --benchmark-options \
    '--csv bench/criterion/bench.csv --output bench/criterion/bench.html'
fi

if [[ 1 == $tasty ]]; then
  cabal bench bench-tasty --benchmark-options \
    '--csv bench/tasty/bench.csv --svg bench/tasty/bench.svg'
fi