# syntax=docker/dockerfile:1.7
# --- Stage 1: builder ---
FROM rocker/r-ver:4.5.3 AS builder

WORKDIR /build

# System dependencies for compiling R packages (shared with dev setup + CI)
COPY dev/install-system-deps.sh dev/install-system-deps.sh
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean && \
    bash dev/install-system-deps.sh

# Restore only runtime packages (and their transitive deps) from the lockfile.
# Dev tools like devtools/roxygen2/pkgdown/testthat are skipped.
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
RUN --mount=type=cache,target=/root/.cache/R/renv,sharing=locked \
    Rscript -e 'source("renv/activate.R"); \
  renv::restore(prompt = FALSE, packages = c( \
    "cli", "rlang", \
    "shiny", "bslib", "ggplot2", \
    "flexsurv", "future", "jsonlite", "promises" \
  )); \
  lib <- renv::paths$library(); \
  pkgs <- list.dirs(lib, full.names = TRUE, recursive = FALSE); \
  file.copy(pkgs, "/usr/local/lib/R/site-library", recursive = TRUE)'

# Install the package itself (bypass renv so it goes to site-library)
COPY . .
RUN R CMD INSTALL --no-multiarch --with-keep.source --library=/usr/local/lib/R/site-library .

# --- Stage 2: runtime ---
FROM rocker/r-ver:4.5.3

# Runtime system libraries only (no -dev packages)
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean && \
    apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libcurl4 \
    libfontconfig1 \
    libfreetype6 \
    libfribidi0 \
    libharfbuzz0b \
    libjpeg-turbo8 \
    libpng16-16t64 \
    libtiff6 \
    libuv1 \
    libwebp7 \
    libxml2

# Copy installed R library from builder
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=builder /usr/local/lib/R/library /usr/local/lib/R/library

# Non-root user
RUN useradd --create-home --uid 1000 shiny
USER shiny

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD curl -f http://localhost:3838/ || exit 1

CMD ["R", "-e", "iftp::run_app(options = list(host = '0.0.0.0', port = 3838L))"]
