# build and test
FROM amd64/alpine as build-stage
LABEL stage=build-stage

ARG path_size_vers

RUN mkdir -p /opt/path-size
ADD . /opt/path-size

RUN <<EOF
set -e

# Taken from haskell/ghcup-hs's dockerfile
apk add --no-cache \
  curl \
  g++ \
  gcc \
  git \
  binutils \
  binutils-gold \
  coreutils \
  bsd-compat-headers \
  gmp \
  gmp-dev \
  libffi-dev \
  ncurses-dev \
  make \
  perl \
  tar \
  xz \
  zlib \
  zlib-dev \
  zlib-static

echo "*** Installing ghcup ***"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh

echo 'PATH=$PATH:$HOME/.ghcup/bin' >> $HOME/.bashrc

. $HOME/.bashrc

echo "*** Installing ghc ***"
ghcup install ghc 9.8.2 --set

echo "*** Installing cabal ***"
ghcup install cabal 3.10.3.0 --set

echo "*** Building path-size ***"

cd /opt/path-size

# NOTE: [Cabal Build vs. Install]
#
# Replace 'cabal build' with 'cabal install' once --enable-executable-static
# works for install.

cabal update
cabal build --enable-executable-static --enable-executable-stripping path-size:exe:path-size --ghc-options -Werror

echo "*** Copy binary ***"

mkdir -p /opt/path-size/bin

arch=$(uname -m)

cp ./dist-newstyle/build/x86_64-linux/ghc-*/path-size-*/x/path-size/opt/build/path-size/path-size "/opt/path-size/bin/path-size_$path_size_vers-$arch-linux-static"

cd ../../

EOF

# export executable
FROM scratch as export-stage
LABEL stage=export-stage
COPY --from=build-stage /opt/path-size/bin/* /
