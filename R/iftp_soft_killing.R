# Internal weighted quantile -------------------------------------------------

weighted_cost_quantile <- function(values, prob, weights, cost) {
  if (length(values) != length(weights)) abort("values and weights lengths must match.")
  if (prob <= 0) {
    return(values[which.min(cost(values))])
  }
  if (prob >= 1) {
    return(values[which.max(cost(values))])
  }

  total_weight <- sum(weights)
  sort_order <- order(cost(values))
  values_sorted <- values[sort_order]
  weights_sorted <- weights[sort_order]

  cumulative_weight <- cumsum(weights_sorted)
  threshold_idx <- which(cumulative_weight >= prob * total_weight)[1L]

  if (is.na(threshold_idx)) {
    return(values[which.max(cost(values))])
  }
  values_sorted[threshold_idx]
}

# Soft-killing IFPT ----------------------------------------------------------

#' Soft-killing IFPT boundary approximation
#'
#' Approximates the boundary of the inverse first-passage time problem using
#' the soft-killing approach from Klump & Kolb (2022). Particles are simulated
#' on an equidistant time grid and assigned weights that decrease through soft
#' killing, rather than hard removal.
#'
#' @param cdf Target CDF function or an `iftp_dist` object (created by
#'   `dist_exponential()` etc.). Referred to as
#'   `Fg` (hazard-related CDF) in the original paper.
#' @param time_horizon Time horizon (positive number).
#' @param killing_rate Soft killing rate. Defaults to 1.
#' @param time_steps Number of equidistant time steps.
#' @param n_particles Number of Monte Carlo particles.
#' @param process Transition kernel created by [process_bm()] or a custom
#'   function with signature `function(x, dt)` that maps current
#'   positions to new positions. Defaults to standard Brownian motion.
#' @param cost Cost function mapping particle positions to a selection
#'   criterion. Equivalent to the negative fitness function \code{-F} from
#'   Klump (2022, Section 2.4.1). Particles with \emph{high} \code{cost(x)}
#'   are penalised (their weights are reduced). Defaults to
#'   \code{function(x) -x}, which penalises the most-negative particles and
#'   yields a \emph{lower} boundary. Use \code{identity} for an upper
#'   boundary or \code{abs} for a two-sided (symmetric) boundary.
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
#'     The penalised region is
#'     \code{\{(t, x) : cost(x) >= cost(b(t))\}}. \code{NA} at time 0.}
#'   \item{params}{List of simulation parameters including \code{process},
#'     \code{init}, and \code{cost}.}
#' }
#'
#' @details
#' Unlike the Anulova and quantile discretizations which remove particles
#' (hard killing), this method reduces particle weights when they exceed
#' the threshold in cost space (soft killing). The weighted quantile of
#' \code{cost(x)} determines the threshold at each step.
#'
#' The time grid consists of `time_steps + 1` equidistant points with spacing
#' `time_horizon / time_steps`.
#'
#' The algorithm:
#' 1. Initialise `n_particles` particles from `init` with equal weights.
#' 2. At each time step, advance particles via the transition kernel.
#' 3. Compute the conditional survival ratio
#'    `alpha = (1 - cdf(t_{i+1})) / (1 - cdf(t_i))`.
#' 4. Find the `(1 - p)`-quantile of `cost(x)` (the survival quantile).
#' 5. Reduce weights of particles where `cost(x) >= threshold` by factor
#'    `exp(-killing_rate * dt)`.
#'
#' @examples
#' result <- iftp_soft_killing(
#'   cdf = dist_exponential(), time_horizon = 2,
#'   time_steps = 16, n_particles = 1000, seed = 42
#' )
#' head(result$boundary)
#'
#' @references
#' Klump, A. and Kolb, M. (2024). An elementary approach to the inverse
#' first-passage-time problem for soft-killed Brownian motion. *Journal of
#' Applied Probability*, 61(1), 279--300. \doi{10.1017/jpr.2023.39}
#'
#' @family iftp
#' @export
iftp_soft_killing <- function(cdf, time_horizon,
                              killing_rate = 1, time_steps, n_particles,
                              process = process_bm(),
                              cost = function(x) -x,
                              init = function(n) rep(0, n),
                              capture_seed = FALSE,
                              seed = NULL) {
  distribution <- extract_dist_name(cdf)
  cdf <- extract_cdf(cdf)
  check_positive_scalar(time_horizon, "time_horizon")
  check_positive_scalar(killing_rate, "killing_rate")
  check_positive_integer(time_steps, "time_steps")
  check_positive_integer(n_particles, "n_particles")
  validate_process(process)
  validate_cost(cost)
  validate_init(init)

  seed <- resolve_seed(seed, capture_seed)

  n_points <- time_steps + 1L
  time_grid <- seq(0, time_horizon, length.out = n_points)

  boundary_values <- rep(NA_real_, n_points)
  x <- init(n_particles)
  weights <- rep(1 / n_particles, n_particles)

  cli_progress_bar("Soft-killing IFPT", total = n_points - 1L)

  for (i in seq_len(n_points - 1L)) {
    dt <- time_grid[i + 1L] - time_grid[i]
    x <- process(x, dt)
    if (i == 1L && any(!is.finite(x))) {
      abort("`process` produced non-finite values on the first step.")
    }

    survival_prev <- 1 - cdf(time_grid[i])
    if (survival_prev <= 0) {
      # CDF has reached 1 — remaining thresholds stay NA
      cli_progress_update(set = n_points - 1L)
      break
    }
    survival_ratio <- pmin(pmax(
      (1 - cdf(time_grid[i + 1L])) / survival_prev, 0
    ), 1)
    survival_quantile <- pmin(pmax(
      (survival_ratio - exp(-killing_rate * dt)) / (1 - exp(-killing_rate * dt)), 0
    ), 1)

    costs <- cost(x)
    boundary_values[i + 1L] <- weighted_cost_quantile(x, survival_quantile, weights, cost)

    penalised <- costs >= cost(boundary_values[i + 1L])
    weights[penalised] <- exp(-killing_rate * dt) * weights[penalised]
  }

  cli_progress_done()

  new_iftp_result(
    boundary_df = data.frame(
      time = time_grid,
      boundary = boundary_values
    ),
    params = list(
      algorithm = "soft_killing",
      distribution = distribution,
      time_horizon = time_horizon, killing_rate = killing_rate,
      time_steps = time_steps, n_particles = n_particles,
      process = process, init = init, cost = cost,
      seed = seed
    )
  )
}
