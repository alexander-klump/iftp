# iftp

Simulation tools for the **inverse first-passage time problem** (IFTP) and
**N-branching Markov process** (N-BMP).

Implements three IFTP boundary approximation algorithms:

- **Anulova discretization** — equidistant time grid, quantile-based particle removal
- **Quantile discretization** — non-equidistant time grid from quantile function
- **Soft-killing IFPT** — equidistant time grid, weighted particles with soft killing (no hard removal)

Plus two particle system simulations:

- **N-BMP** — N particles with Poisson branching events; worst particle (largest cost) removed and replaced by a copy of a random other particle
- **Quantile thinning (QTPS)** — N particles on a quantile-derived time grid; worst particle permanently removed at each step, no replacement

An interactive Shiny application is also included.

## Getting started

### Install

```r
# install.packages("pak")
pak::pak("alexander-klump/iftp")

# Pin to a specific release or commit for reproducibility:
pak::pak("alexander-klump/iftp@v0.1.0")   # tag
pak::pak("alexander-klump/iftp@1cf2acb")  # commit SHA
```

### IFTP boundary approximation

Approximate the boundary for an exponential first-passage time distribution
using 1000 Brownian particles:

```r
library(iftp)

result <- iftp_anulova(
  cdf = dist_exponential(rate = 1),
  time_horizon = 2,
  time_steps = 100,
  n_particles = 1000,
  seed = 42
)

result
#> IFTP result (Anulova discretization)
#>   Time steps: 100 | Particles: 1000 | Horizon: 2

plot(result)
```

Other algorithms work the same way — just swap the function:

```r
# Quantile discretization (non-equidistant grid)
result_q <- iftp_quantile(
  cdf = dist_exponential(rate = 1),
  time_horizon = 2,
  time_steps = 100,
  remove_per_step = 10,
  seed = 42
)

# Soft-killing IFPT (weighted particles)
result_sk <- iftp_soft_killing(
  cdf = dist_exponential(rate = 1),
  time_horizon = 2,
  killing_rate = 1,
  time_steps = 100,
  n_particles = 1000,
  seed = 42
)
```

Available target distributions: `dist_exponential()`, `dist_weibull()`,
`dist_gamma()`, `dist_uniform()`, `dist_frechet()`, `dist_lomax()`,
`dist_log_logistic()`, `dist_arcsine()`, or pass any CDF function directly.

### N-BMP particle system

Simulate 50 particles with branching and selection:

```r
ps <- nbmp_simulate(
  n_particles = 50,
  time_horizon = 5,
  branching_rate = 1,
  seed = 42
)

plot(ps)
```

### Shiny app

Launch the interactive UI to explore all algorithms and parameter settings:

```r
# from the R console (package installed)
iftp::run_app()
```

```bash
# from the terminal (installed package)
Rscript -e 'iftp::run_app()'

# from the repo directory (development mode)
Rscript -e 'devtools::load_all(); run_app()'
```

Or run it with Docker:

```bash
docker build -t iftp-shiny .
docker run -p 3838:3838 iftp-shiny
# Open http://localhost:3838
```

## References

- Klump & Savov (2025) — [IFPT for Levy processes and diffusions](https://doi.org/10.1214/25-AAP2157)
- Klump (2023) — [IFPT as hydrodynamic limit](https://doi.org/10.1007/s11009-023-10020-7)
- Klump & Kolb (2024) — [Soft-killing IFPT](https://doi.org/10.1017/jpr.2023.39)
- De Masi et al. (2019) — [N-BBM hydrodynamics](https://doi.org/10.1007/978-3-030-15096-9_18)
- Berard & Frenais (2023) — [N-BMP hydrodynamics](https://arxiv.org/abs/2311.12453)
- Klump (2022) — [PhD thesis: IFPT via stochastic order](https://doi.org/10.17619/UNIPB/1-1648)

## Development setup

### System dependencies (Ubuntu/Debian)

```bash
sudo bash dev/install-system-deps.sh
```

### macOS

```bash
brew install freetype libpng libtiff zlib libjpeg-turbo webp harfbuzz fribidi libuv libgit2 pandoc pari-gp
```

### R setup

```bash
git clone https://github.com/alexander-klump/iftp.git
cd iftp

Rscript -e 'install.packages("renv")'
Rscript -e 'renv::restore()'

Rscript -e 'devtools::load_all()'       # Load package
Rscript -e 'devtools::test()'           # Run tests
Rscript -e 'lintr::lint_package()'      # Lint
Rscript -e 'styler::style_pkg()'        # Format
Rscript -e 'devtools::check()'          # Full R CMD check
Rscript -e 'devtools::document()'       # Build docs
```

## Deployment

### Docker

```bash
docker build -t iftp-shiny .
docker run -d -p 3838:3838 --restart unless-stopped --name iftp iftp-shiny
```

Access at <http://localhost:3838>. Stop with `docker stop iftp`.

## License

MIT
