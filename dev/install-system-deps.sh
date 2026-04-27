#!/usr/bin/env bash
# OS libraries required to compile the R packages used by this project.
# Single source of truth for: README dev setup, renv-update CI, Dockerfile builder.

set -euo pipefail

SUDO=""
if [ "$(id -u)" -ne 0 ]; then
  SUDO="sudo"
fi

$SUDO apt-get update
$SUDO apt-get install -y --no-install-recommends \
  cmake \
  curl \
  pandoc \
  pari-gp \
  libcurl4-openssl-dev \
  libfontconfig1-dev \
  libfreetype6-dev \
  libfribidi-dev \
  libgit2-dev \
  libharfbuzz-dev \
  libjpeg-dev \
  libpng-dev \
  libssl-dev \
  libtiff-dev \
  libuv1-dev \
  libwebp-dev \
  libxml2-dev \
  zlib1g-dev
