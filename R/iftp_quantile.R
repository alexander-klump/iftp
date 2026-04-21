#' Quantile discretization for the IFTP
#'
#' Approximates the boundary of the inverse first-passage time problem using
#' a non-equidistant time grid derived from the target distribution's quantile
#' function. A fixed number of particles are removed at each step.
#'
#' @param cdf Target CDF function or an `iftp_dist` object (created by
#'   `dist_exponential()` etc.).
#' @param qf Quantile function of the target distribution. If `cdf` is an
#'   `iftp_dist` object with a quantile function, it is used automatically.
#' @param time_horizon Time horizon (positive number).
#' @param time_steps Refinement parameter. The quantile function is evaluated at
#'   multiples of `1/time_steps`.
#' @param remove_per_step Number of particles removed per step. The total sample
#'   size is `time_steps * remove_per_step`.
#' @param process Transition kernel created by [process_bm()] or a custom
#'   function with signature `function(x, dt)` that maps current
#'   positions to new positions. Defaults to standard Brownian motion.
#' @param cost Cost function applied to particle positions before computing
#'   quantiles. Must be a vectorised numeric-to-numeric function.
#'   Equivalent to the negative fitness function \code{-F} from
#'   Klump (2022, Section 2.4.1). Use `abs` (default) for a two-sided
#'   (symmetric) boundary, or `identity` for a one-sided (upper) boundary.
#' @param init Initial distribution function. A function taking a single
#'   argument `n` and returning `n` starting positions. Defaults to
#'   `function(n) rep(0, n)` (all particles at the origin).
#' @param method Grid construction method: `"quantiles"` (default) builds the
#'   time grid deterministically from the quantile function;
#'   `"order_statistics"` samples from the distribution and uses sorted values
#'   as the grid. The latter requires an `iftp_dist` object with a sampling
#'   function.
#' @param capture_seed Logical. If `TRUE` and `seed` is `NULL`, a random
#'   seed is generated, set, and stored in `result$params$seed` for
#'   post-hoc reproducibility. Defaults to `FALSE`.
#' @param seed Optional integer seed for reproducibility. When provided,
#'   it is always stored in `result$params$seed`.
#'
#' @return An `iftp_result` object (a list with elements `boundary` and
#'   `params`). `boundary` is a data frame with columns:
#' \describe{
#'   \item{time}{Non-equidistant time points derived from the quantile
#'     function.}
#'   \item{boundary}{Particle positions at the survival threshold.
#'     The survival region is \code{\{(t, x) : cost(x) <= cost(b(t))\}}.}
#' }
#'
#' @details
#' Unlike the Anulova discretization, which uses an equidistant time grid,
#' this method computes the time grid from the quantile function of the target
#' distribution. At each step, exactly `remove_per_step` particles are removed
#' (those with the largest values of `cost(X)`), giving a fixed reduction per
#' step.
#'
#' The time grid is `t_k = qf(k/time_steps)` for
#' `k = 1, ..., floor(cdf(time_horizon)*time_steps)`, where
#' `qf` is the quantile function of the target distribution.
#'
#' The boundary at each step is the `(n_alive - remove_per_step) / n_alive` quantile
#' of `cost(X)` among surviving particles, and particles with
#' `cost(X) > boundary` are removed.
#'
#' @examples
#' d <- dist_exponential(rate = 1)
#' result <- iftp_quantile(
#'   cdf = d, time_horizon = 2, time_steps = 50,
#'   remove_per_step = 20, seed = 42
#' )
#' head(result$boundary)
#'
#' @references
#' Klump, A. (2022). *The classical and the soft-killing Inverse First-Passage
#' Time Problem: a stochastic order approach*. PhD thesis, University of Paderborn. Section 2.4.3.
#'
#' @family iftp
#' @export
iftp_quantile <- function(cdf, qf = NULL, time_horizon, time_steps,
                          remove_per_step,
                          process = process_bm(),
                          cost = abs,
                          init = function(n) rep(0, n),
                          method = c("quantiles", "order_statistics"),
                          capture_seed = FALSE,
                          seed = NULL) {
  method <- match.arg(method)
  cdf_orig <- cdf
  distribution <- extract_dist_name(cdf_orig)
  rf <- extract_sample(cdf_orig)
  cdf <- extract_cdf(cdf_orig)
  if (method == "quantiles") {
    qf <- extract_quantile(cdf_orig, qf)
  }
  check_positive_integer(time_steps, "time_steps")
  check_positive_integer(remove_per_step, "remove_per_step")
  check_positive_scalar(time_horizon, "time_horizon")
  validate_process(process)
  validate_cost(cost)
  validate_init(init)

  seed <- resolve_seed(seed, capture_seed)

  n_total <- time_steps * remove_per_step

  time_grid <- build_time_grid(
    cdf = cdf, qf = qf, rf = rf,
    n_steps = time_steps, time_horizon = time_horizon, method = method
  )

  n_grid_points <- length(time_grid)
  boundary_values <- numeric(n_grid_points)
  x <- init(n_total)
  n_alive <- n_total

  cli_progress_bar("Quantile discretization", total = n_grid_points - 1L)

  for (i in seq_along(time_grid)[-1]) {
    dt <- time_grid[i] - time_grid[i - 1]
    x <- process(x, dt)
    if (i == 2L && any(!is.finite(x))) {
      abort("`process` produced non-finite values on the first step.")
    }

    if (n_alive <= remove_per_step) {
      cli::cli_warn(paste0(
        "Insufficient particles at step {i}: {n_alive} alive, ",
        "{remove_per_step} to remove. Stopping early."
      ))
      break
    }

    costs <- cost(x)
    cost_threshold <- stats::quantile(
      costs, (n_alive - remove_per_step) / n_alive
    )
    keep <- costs <= cost_threshold
    boundary_values[i] <- x[keep][which.max(costs[keep])]
    x <- x[keep]

    n_alive <- length(x)
    cli_progress_update()
  }

  cli_progress_done()

  new_iftp_result(
    boundary_df = data.frame(time = time_grid, boundary = boundary_values),
    params = list(
      algorithm = "quantile",
      distribution = distribution,
      time_steps = time_steps, remove_per_step = remove_per_step,
      time_horizon = time_horizon,
      method = method,
      process = process, init = init, cost = cost,
      seed = seed
    )
  )
}
