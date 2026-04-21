#' Simulate quantile thinning particle system (QTPS)
#'
#' Simulates a particle system where N particles perform independent
#' Brownian motions on a quantile-derived time grid. At each step, the
#' particle with the largest absolute value is permanently removed
#' (no replacement), so the particle count decreases from N to 1.
#'
#' @param n_particles Number of initial particles (at least 2).
#' @param cdf Target CDF function or an `iftp_dist` object (created by
#'   `dist_exponential()` etc.).
#' @param qf Quantile function of the target distribution. If `cdf` is an
#'   `iftp_dist` object with a quantile function, it is used automatically.
#' @param time_horizon Time horizon (positive number).
#' @param process Transition kernel created by [process_bm()] or a custom
#'   function with signature `function(x, dt)` that maps current
#'   positions to new positions. Defaults to standard Brownian motion.
#' @param cost Cost function applied to particle positions to determine which
#'   particle is removed at each step. Equivalent to the negative fitness
#'   function \code{-F} from Klump (2022, Section 2.4.1). Must be a function
#'   mapping a numeric vector to a numeric vector of the same length.
#'   Defaults to `abs` (two-sided boundary).
#' @param init Initial distribution function. A function taking a single
#'   argument `n` and returning `n` starting positions. Defaults to
#'   `function(n) rep(0, n)` (all particles at the origin).
#' @param method Grid construction method: `"quantiles"` (default) builds the
#'   time grid deterministically from the quantile function;
#'   `"order_statistics"` samples from the distribution and uses sorted values
#'   as the grid. The latter requires an `iftp_dist` object with a sampling
#'   function.
#' @param path_resolution Optional positive number. When supplied, each
#'   inter-event interval is sub-divided into steps of at most
#'   `path_resolution` time units, producing a finer output time grid.
#'   `is_event` marks only the original deletion steps. Defaults to `NULL`
#'   (no sub-stepping).
#' @param capture_seed Logical. If `TRUE` and `seed` is `NULL`, a random
#'   seed is generated, set, and stored in `result$params$seed` for
#'   post-hoc reproducibility. Defaults to `FALSE`.
#' @param seed Optional integer seed for reproducibility. When provided,
#'   it is always stored in `result$params$seed`.
#'
#' @return A `ps_result` object (S3 class) containing:
#' \describe{
#'   \item{type}{Character: `"qtps"`.}
#'   \item{times}{Numeric vector of recorded time points (starting at 0).}
#'   \item{is_event}{Logical vector. `TRUE` at deletion steps; `FALSE` at
#'     time 0 and at sub-steps inserted by `path_resolution`.}
#'   \item{positions}{Matrix of size `(n_rows x n_particles)` with particle
#'     positions at each time point; dead particles are `NA`.}
#'   \item{params}{List of simulation parameters: `n_particles`, `time_horizon`,
#'     `method`, `path_resolution`, `process`, `init`, and `cost`.}
#' }
#'
#' @details
#' The time grid is `t_k = qf(k/N)` for `k = 1, ..., floor(cdf(time_horizon)*N)`.
#' At each step, all surviving particles receive independent increments
#' from the stochastic process, and the particle with the largest cost(X)
#' is permanently removed.
#'
#' Internally, a fixed-length alive-mask is used so that particle indices
#' remain stable across steps, enabling the full position matrix.
#'
#' @examples
#' result <- qtps_simulate(
#'   n_particles = 100, cdf = dist_exponential(), time_horizon = 2, seed = 42
#' )
#' head(result$positions)
#'
#' @references
#' Klump, A. (2023). The inverse first passage time problem as hydrodynamic
#' limit of a particle system. *Methodology and Computing in Applied
#' Probability*, 25, 42. \doi{10.1007/s11009-023-10020-7}
#'
#' @seealso [plot_ps_result()] for visualization
#' @family nbmp
#' @export
qtps_simulate <- function(n_particles, cdf, qf = NULL, time_horizon, # nolint: object_name_linter.
                          process = process_bm(),
                          cost = abs,
                          init = function(n) rep(0, n),
                          method = c("quantiles", "order_statistics"),
                          path_resolution = NULL,
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
  check_positive_integer(n_particles, "n_particles")
  if (n_particles < 2L) abort("`n_particles` must be at least 2.")
  check_positive_scalar(time_horizon, "time_horizon")
  validate_process(process)
  validate_cost(cost)
  validate_init(init)
  if (!is.null(path_resolution)) {
    check_positive_scalar(path_resolution, "path_resolution")
  }

  seed <- resolve_seed(seed, capture_seed)

  time_grid <- build_time_grid(
    cdf = cdf, qf = qf, rf = rf,
    n_steps = n_particles, time_horizon = time_horizon, method = method
  )

  x <- init(n_particles)

  deltas <- diff(time_grid)
  sim <- qtps_loop(
    x, deltas, n_particles,
    path_resolution %||% Inf, process, cost
  )

  new_ps_result(
    type = "qtps",
    times = sim$times,
    is_event = sim$is_event,
    positions = sim$positions,
    params = list(
      n_particles = n_particles, time_horizon = time_horizon,
      distribution = distribution,
      method = method,
      path_resolution = path_resolution,
      process = process, init = init, cost = cost,
      seed = seed
    )
  )
}

# Internal: unified QTPS simulation loop.
# Expands inter-event deltas into sub-step deltas, runs a flat loop, and
# deletes at event boundaries.  path_resolution = Inf ≡ no sub-stepping.
qtps_loop <- function(x, deltas, n_particles,
                      path_resolution, process, cost) {
  alive <- rep(TRUE, n_particles)
  n_events <- length(deltas)
  grid <- expand_substeps(deltas, path_resolution, n_events)
  step_deltas <- grid$step_deltas
  total_rows <- grid$total_rows
  times <- grid$times
  is_event <- grid$is_event

  col_map <- seq_len(n_particles)
  pos_segs <- init_segments(total_rows, x)

  cli_progress_bar("QTPS simulation", total = n_events)

  for (row in seq.int(2L, total_rows)) {
    living <- which(alive)
    # Capture result before subset assignment (preserves jump_size attr for Task 5)
    x_new <- process(x[living], step_deltas[row - 1L])
    if (row == 2L && any(!is.finite(x_new))) {
      abort("`process` produced non-finite values on the first step.")
    }
    x[living] <- x_new

    # Jump-segment breaks (extract from x_new, not x — subset assignment strips attrs)
    jump_size <- attr(x_new, "jump_size")
    if (!is.null(jump_size)) {
      jumped <- which(jump_size != 0)
      for (j in jumped) {
        i <- living[j]
        pre_jump <- x[i] - jump_size[j]
        pos_segs <- end_and_append_segment(
          pos_segs, col_map[i], row,
          pre_value = pre_jump,
          post_value = x[i]
        )
        col_map[i] <- pos_segs$idx
      }
    }

    if (is_event[row]) {
      costs_living <- cost(x[living])
      killed_local <- which.max(costs_living)
      killed_idx <- living[killed_local]

      # Write all positions (including killed particle's death position)
      for (i in living) {
        pos_segs$cols[[col_map[i]]][row] <- x[i]
      }

      alive[killed_idx] <- FALSE
      x[killed_idx] <- NA_real_

      cli_progress_update()
    } else {
      for (i in living) {
        pos_segs$cols[[col_map[i]]][row] <- x[i]
      }
    }
  }

  cli_progress_done()

  list(
    times = times, is_event = is_event,
    positions = finalize_segments(pos_segs)
  )
}
