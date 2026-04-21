#' Anulova discretization for the IFTP
#'
#' Approximates the boundary of the inverse first-passage time problem using
#' the Anulova discretization on an equidistant time grid. Particles are
#' simulated from a stochastic process and removed at each step according to
#' the target distribution's conditional quantiles.
#'
#' @param cdf Target CDF function or an `iftp_dist` object (created by
#'   `dist_exponential()` etc.).
#' @param time_horizon Time horizon (positive number).
#' @param time_steps Number of equidistant time steps.
#' @param n_particles Sample size (number of initial particles). Must be at
#'   least `time_steps`.
#' @param process Transition kernel created by [process_bm()] or a custom
#'   function with signature `function(x, dt)` that maps current
#'   positions to new positions. Defaults to standard Brownian motion.
#' @param cost Cost function mapping particle positions to a selection
#'   criterion. The particle with the largest \code{cost(x)} is removed
#'   (selected against). Equivalent to the negative fitness function
#'   \code{-F} from Klump (2022, Section 2.4.1). Common choices: \code{abs}
#'   (default) for two-sided (symmetric) boundaries, \code{identity} for
#'   one-sided (upper) boundaries.
#' @param init Initial distribution function. A function taking a single
#'   argument `n` and returning `n` starting positions. Defaults to
#'   `function(n) rep(0, n)` (all particles at the origin).
#' @param capture_seed Logical. If `TRUE` and `seed` is `NULL`, a random
#'   seed is generated, set, and stored in `result$params$seed` for
#'   post-hoc reproducibility. Defaults to `FALSE`.
#' @param seed Optional integer seed for reproducibility. When provided,
#'   it is always stored in `result$params$seed`.
#'
#' @return An \code{iftp_result} object containing:
#' \describe{
#'   \item{boundary}{Data frame with \code{time} and \code{boundary} columns.
#'     Boundary values are particle positions at the survival threshold.
#'     The survival region is \code{\{(t, x) : cost(x) <= cost(b(t))\}}.}
#'   \item{params}{List of simulation parameters including \code{process},
#'     \code{init}, and \code{cost}.}
#' }
#'
#' @details
#' The algorithm works as follows:
#' 1. Initialise `N` particles at the origin.
#' 2. At each time step `i`, advance all surviving particles by adding
#'    independent increments from the process.
#' 3. Compute the conditional survival probability
#'    `alpha = (1 - cdf(t_i)) / (1 - cdf(t_{i-1}))`.
#' 4. Compute the \code{alpha}-quantile of \code{cost(X)} among surviving particles.
#' 5. Remove particles where \code{cost(X)} exceeds the quantile.
#'
#' The resulting quantiles form the boundary approximation.
#'
#' @examples
#' # Exponential distribution with standard BM
#' result <- iftp_anulova(dist_exponential(),
#'   time_horizon = 2, time_steps = 100,
#'   n_particles = 1000, seed = 42
#' )
#' head(result$boundary)
#'
#' @references
#' Anulova, S. V. (1981). On Markov stopping times with a given distribution
#' for a Wiener process. *Theory of Probability & Its Applications*, 25(2),
#' 362--366. \doi{10.1137/1125045}
#'
#' Klump, A. (2022). *The classical and the soft-killing Inverse First-Passage
#' Time Problem: a stochastic order approach*. PhD thesis, University of Paderborn. Section 2.4.3.
#'
#' Klump, A. and Savov, M. (2025). Conditions for existence and uniqueness
#' of the inverse first-passage time problem applicable for \enc{Lévy}{Levy} processes
#' and diffusions. *Ann. Appl. Probab.*, 35(3), 1791--1827.
#' \doi{10.1214/25-AAP2157}
#'
#' @family iftp
#' @export
iftp_anulova <- function(cdf, time_horizon, time_steps,
                         n_particles, # nolint: object_name_linter.
                         process = process_bm(),
                         cost = abs,
                         init = function(n) rep(0, n),
                         capture_seed = FALSE,
                         seed = NULL) {
  distribution <- extract_dist_name(cdf)
  cdf <- extract_cdf(cdf)
  check_positive_scalar(time_horizon, "time_horizon")
  check_positive_integer(time_steps, "time_steps")
  check_positive_integer(n_particles, "n_particles")
  validate_process(process)
  validate_cost(cost)
  validate_init(init)

  if (n_particles < time_steps) {
    abort(
      "`n_particles` (sample size) must be at least `time_steps`."
    )
  }

  seed <- resolve_seed(seed, capture_seed)

  dt <- time_horizon / time_steps
  boundary_values <- numeric(time_steps)
  x <- init(n_particles)

  cli_progress_bar("Anulova discretization", total = time_steps)

  for (i in seq_len(time_steps)) {
    survival_next <- 1 - cdf(i * dt)

    if (survival_next > 0) {
      x <- process(x, dt)
      if (i == 1L && any(!is.finite(x))) {
        abort("`process` produced non-finite values on the first step.")
      }
      survival_ratio <- survival_next / (1 - cdf((i - 1) * dt))

      costs <- cost(x)
      cost_threshold <- stats::quantile(costs, survival_ratio)
      keep <- costs <= cost_threshold
      if (any(keep)) {
        boundary_values[i] <- x[keep][which.max(costs[keep])]
      }
      x <- x[keep]

      if (length(x) == 0L) {
        cli::cli_warn(
          "All particles depleted at step {i}/{time_steps}. Boundary set to 0 for remaining steps."
        )
        boundary_values[i:time_steps] <- 0
        cli_progress_update(set = time_steps)
        break
      }
    } else {
      boundary_values[i:time_steps] <- 0
      cli_progress_update(set = time_steps)
      break
    }

    cli_progress_update()
  }

  cli_progress_done()

  new_iftp_result(
    boundary_df = data.frame(
      time = seq(0, time_horizon, by = dt),
      boundary = c(0, boundary_values)
    ),
    params = list(
      algorithm = "anulova",
      distribution = distribution,
      n_particles = n_particles, time_horizon = time_horizon,
      time_steps = time_steps,
      process = process, init = init, cost = cost,
      seed = seed
    )
  )
}
