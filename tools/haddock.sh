set -e

export LANG="C.UTF-8"

cabal haddock --haddock-hyperlink-source --haddock-quickjump

mkdir -p docs/

# shellcheck disable=SC2038
find docs/ -type f | xargs -I % sh -c "rm -r %"

cp -r dist-newstyle/build/x86_64-linux/ghc-*/path-size-0.1/opt/doc/html/path-size/* docs/