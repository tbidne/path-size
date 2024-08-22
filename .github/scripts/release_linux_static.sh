#!/usr/bin/env bash

set -e

path_size_vers="0.1"

dir=$1

mkdir -p bin

docker build \
  -t path-size_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg path-size_vers=$path_size_vers \
  .

cp docker_out/path-size_* bin/
