#' iftp: Inverse First-Passage Time Simulations and Particle Systems
#'
#' Simulation tools for the inverse first-passage time problem (IFTP) and
#' related particle systems such as the N-branching Markov process (N-BMP)
#' and the quantile thinning particle system (QTPS). Implements three IFTP
#' boundary approximation algorithms, two particle system simulations, and
#' an interactive Shiny application.
#'
#' @section IFTP Algorithms:
#' \describe{
#'   \item{[iftp_anulova()]}{Anulova discretization — equidistant time grid,
#'     quantile-based particle removal. Supports BM,
#'     BM+drift+Poisson, negative Gamma, and diffusion processes.}
#'   \item{[iftp_quantile()]}{Quantile discretization — non-equidistant time
#'     grid derived from the target distribution's quantile function. Supports
#'     quantiles and order-statistics methods.}
#'   \item{[iftp_soft_killing()]}{Soft-killing IFPT — weighted particles with
#'     soft killing rate on an equidistant mesh. Based on Klump & Kolb (2024).}
#' }
#'
#' @section Stochastic Processes:
#' \enc{Lévy}{Levy} processes: [process_bm()], [process_bm_drift_poisson()],
#' [process_negative_gamma()],
#' [process_cauchy()].
#' Diffusions (Definition 2.15, Klump & Savov 2025): [process_ou()],
#' [process_cev()], [process_cir()], [process_bessel()],
#' [process_wright_fisher()].
#'
#' @section Particle Systems:
#' \describe{
#'   \item{[nbmp_simulate()]}{N-branching Markov process — N particles
#'     following independent stochastic processes with branching and
#'     fitness-based selection. Returns a \code{ps_result} with full
#'     position matrix.}
#'   \item{[qtps_simulate()]}{Quantile thinning particle system — progressive
#'     deletion without replacement on a non-equidistant time grid. Returns a
#'     \code{ps_result} with position matrix (dead particles become NA).}
#' }
#'
#' @section References:
#' \itemize{
#'   \item Klump, A. and Savov, M. (2025). Conditions for existence and
#'     uniqueness of the inverse first-passage time problem applicable for
#'     \enc{Lévy}{Levy} processes and diffusions.
#'     \doi{10.1214/25-AAP2157}
#'   \item Klump, A. (2023). The inverse first passage time problem as
#'     hydrodynamic limit of a particle system.
#'     \doi{10.1007/s11009-023-10020-7}
#'   \item Klump, A. and Kolb, M. (2024). An elementary approach to the
#'     inverse first-passage-time problem for soft-killed Brownian motion.
#'     \doi{10.1017/jpr.2023.39}
#'   \item De Masi, A., Ferrari, P.A., Presutti, E. and Soprano-Loto, N.
#'     (2019). Hydrodynamics of the N-BBM process.
#'     \doi{10.1007/978-3-030-15096-9_18}
#'   \item \enc{Bérard}{Berard}, J. and \enc{Frénais}{Frenais}, B. (2023).
#'     Hydrodynamic limit of N-branching Markov processes.
#'     arXiv:2311.12453.
#'   \item Klump, A. (2022). *The classical and the soft-killing Inverse
#'     First-Passage Time Problem: a stochastic order approach*. PhD thesis,
#'     University of Paderborn. \doi{10.17619/UNIPB/1-1648}
#' }
#'
#' @name iftp-package
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang abort .data %||%
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
## usethis namespace: end
NULL
