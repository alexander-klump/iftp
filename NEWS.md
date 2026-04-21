# iftp 0.1.0

Initial release.

## IFTP Boundary Approximation

* Three algorithms: Anulova discretization (`iftp_anulova()`), quantile
  discretization (`iftp_quantile()`), and soft-killing IFPT
  (`iftp_soft_killing()`).
* Nine built-in distribution families: exponential, Weibull, log-logistic,
  Frechet, Lomax, Gamma, uniform, arcsine, and soft-killing constant barrier.
* Configurable `cost` function for symmetric (`abs`) or one-sided (`identity`)
  boundaries.
* Order-statistics method for quantile discretization time grid construction.
* Analytical distribution for soft-killing first-passage time with constant
  barrier (`dist_soft_killing_constant_barrier()`).

## Particle Systems

* N-branching Markov process (`nbmp_simulate()`) with configurable branching
  rate, cost-based selection, and path resolution.
* Quantile thinning particle system (`qtps_simulate()`) with progressive
  deletion and non-equidistant time grids.
* Configurable `cost` function for particle selection criterion across all
  simulations.
* Jump-segment support: processes with isolable jumps (BM+Poisson) produce
  segment breaks at discontinuities.
* Export particle paths to CSV/JSON via `export_paths()`.

## Stochastic Processes

* Six diffusion process factories: Ornstein-Uhlenbeck (`process_ou()`),
  CEV (`process_cev()`), CIR (`process_cir()`), Bessel (`process_bessel()`),
  Wright-Fisher (`process_wright_fisher()`), and Cauchy (`process_cauchy()`).
  Euler-Maruyama discretization with domain-specific boundary handling.

## Shiny Application

* Interactive UI (`run_app()`) for all five simulation types.
* Comparison mode: overlay multiple simulations on one plot.
* Reproducible R code snippet export.

## Infrastructure

* Stochastic process abstraction: BM, BM+drift+Poisson,
  negative Gamma, with support for custom process functions.
* Parallel Monte Carlo via `future`/`furrr` (optional).
* Result caching with `save_result()`/`load_result()`.
