#' Simulate N-branching Markov process
#'
#' Simulates the N-BMP particle system: N particles perform independent
#' stochastic processes. At each event (Poisson process with rate
#' N * branching_rate), the particle with the largest `cost(x)` is removed and
#' replaced by a copy of a randomly chosen other particle.
#'
#' @param n_particles Number of particles (at least 2).
#' @param time_horizon Time horizon (positive number).
#' @param branching_rate Branching rate per particle. Defaults to 1.
#' @param process Transition kernel created by [process_bm()] or a custom
#'   function with signature `function(x, dt)` that maps current
#'   positions to new positions. Defaults to standard Brownian motion.
#' @param cost Cost function applied to particle positions for selection.
#'   Must be a vectorised function mapping positions to numeric values.
#'   Equivalent to the negative fitness function \code{-F} from
#'   Klump (2022, Section 2.4.1). The particle with the largest `cost(x)`
#'   is removed at each event. Defaults to `identity`, matching the
#'   convention in De Masi et al. (2019) where the rightmost particle is
#'   removed.
#' @param init Initial distribution function. A function taking a single
#'   argument `n` and returning `n` starting positions. Defaults to
#'   `function(n) rep(0, n)` (all particles at the origin).
#' @param path_resolution Optional positive number. When set, each inter-event
#'   interval is subdivided into sub-steps of at most this size, recording
#'   intermediate particle positions. When `NULL` (default), only event times
#'   are recorded.
#' @param capture_seed Logical. If `TRUE` and `seed` is `NULL`, a random
#'   seed is generated, set, and stored in `result$params$seed` for
#'   post-hoc reproducibility. Defaults to `FALSE`.
#' @param seed Optional integer seed for reproducibility. When provided,
#'   it is always stored in `result$params$seed`.
#'
#' @return A `ps_result` object (S3 class) containing:
#' \describe{
#'   \item{type}{Character: `"nbmp"`.}
#'   \item{times}{Numeric vector of recorded times (including time 0).
#'     When `path_resolution` is set, includes intermediate sub-step times.}
#'   \item{is_event}{Logical vector: `TRUE` at event times, `FALSE` at
#'     time 0 and intermediate sub-steps.}
#'   \item{positions}{Matrix of particle positions (rows = time points,
#'     columns grow with each event).
#'     At event rows, both the killed particle's death position and the
#'     offspring's birth position are stored.
#'     When the process has jumps (indicated by a `jump_size` attribute),
#'     additional segment breaks occur at jump times. At rows with jump
#'     breaks, more columns may have non-`NA` values than there are live
#'     particles.}
#'   \item{params}{List of simulation parameters.}
#' }
#'
#' @details
#' The N-BMP dynamics are:
#' 1. N particles start at positions given by `init`.
#' 2. Each particle independently follows the stochastic process.
#' 3. Events occur as a Poisson process with rate `N * branching_rate`.
#' 4. At each event, the particle with the largest `cost(x)` is removed and
#'    replaced by a copy of a uniformly chosen other particle.
#'
#' The number of events is Poisson-distributed with mean
#' `time_horizon * n_particles * branching_rate`. After the last event,
#' particles are propagated to time `time_horizon`, so
#' `max(result$times)` always equals `time_horizon`.
#'
#' **Position tracking:** Instead of overwriting killed particles in-place,
#' each kill event stores the killed particle's death position and appends a
#' new column for the offspring. This eliminates false diagonal lines in
#' particle path plots.
#'
#' When `path_resolution` is set, each inter-event interval `[t_i, t_{i+1}]`
#' is split into `ceiling((t_{i+1} - t_i) / path_resolution)` equal sub-steps.
#' The final propagation to `time_horizon` is sub-stepped as well.
#' Intermediate rows have `is_event = FALSE`.
#'
#' @examples
#' result <- nbmp_simulate(n_particles = 50, time_horizon = 2, seed = 42)
#' print(result)
#'
#' @references
#' De Masi, A., Ferrari, P. A., Presutti, E. and Soprano-Loto, N. (2019).
#' Hydrodynamics of the N-BBM process. In: *Stochastic Dynamics Out of
#' Equilibrium* (Giacomin et al., eds.), Springer Proceedings in Mathematics
#' & Statistics, vol. 282, pp. 523--549. \doi{10.1007/978-3-030-15096-9_18}
#'
#' Bérard, J. and Frénais, B. (2023). Hydrodynamic limit of N-branching
#' Markov processes. *arXiv preprint* arXiv:2311.12453.
#'
#' @seealso [plot_ps_result()] for visualization
#' @family nbmp
#' @export
nbmp_simulate <- function(
  # nolint: object_name_linter.
  n_particles, time_horizon = 2, branching_rate = 1,
  process = process_bm(),
  cost = identity,
  init = function(n) rep(0, n),
  path_resolution = NULL,
  capture_seed = FALSE,
  seed = NULL
) {
  check_positive_integer(n_particles, "n_particles")
  if (n_particles < 2L) abort("`n_particles` must be at least 2.")
  check_positive_scalar(time_horizon, "time_horizon")
  check_positive_scalar(branching_rate, "branching_rate")
  validate_process(process)
  validate_cost(cost)
  validate_init(init)
  if (!is.null(path_resolution)) {
    check_positive_scalar(path_resolution, "path_resolution")
  }

  seed <- resolve_seed(seed, capture_seed)

  rate <- n_particles * branching_rate

  x <- init(n_particles)

  tau <- poisson_event_times(time_horizon, rate)
  intervals <- c(tau, max(time_horizon - sum(tau), 0))

  sim <- nbmp_loop(
    x, intervals, n_particles,
    path_resolution %||% Inf, process, cost
  )

  new_ps_result(
    type = "nbmp",
    times = sim$times,
    is_event = sim$is_event,
    positions = sim$positions,
    params = list(
      n_particles = n_particles, time_horizon = time_horizon,
      branching_rate = branching_rate,
      process = process, init = init, cost = cost,
      path_resolution = path_resolution,
      seed = seed
    )
  )
}

# Internal: simulation loop with column-append position tracking.
# At events, the killed particle's column stores its death position,
# then a new column is appended for the offspring. col_map (length N)
# maps current particle indices to column indices.
nbmp_loop <- function(x, intervals, n_particles,
                      path_resolution, process, cost) {
  n_events <- length(intervals) - 1L
  grid <- expand_substeps(intervals, path_resolution, n_events)
  step_deltas <- grid$step_deltas
  total_rows <- grid$total_rows
  times <- grid$times
  is_event <- grid$is_event

  col_map <- seq_len(n_particles)
  pos_segs <- init_segments(total_rows, x)

  if (n_events > 0L) cli_progress_bar("N-BMP simulation", total = n_events)

  for (row in seq.int(2L, total_rows)) {
    x <- process(x, step_deltas[row - 1L])
    if (row == 2L && any(!is.finite(x))) {
      abort("`process` produced non-finite values on the first step.")
    }

    # Jump-segment breaks: split segments at discontinuities
    # (attr preserved here because x <- process(...) is full assignment)
    jump_size <- attr(x, "jump_size")
    if (!is.null(jump_size)) {
      for (i in which(jump_size != 0)) {
        pre_jump <- x[i] - jump_size[i]
        pos_segs <- end_and_append_segment(
          pos_segs, col_map[i], row,
          pre_value = pre_jump,
          post_value = x[i]
        )
        col_map[i] <- pos_segs$idx
      }
    }

    if (is_event[row]) {
      costs <- cost(x)
      killed_idx <- which.max(costs)

      candidates <- seq_len(n_particles)[-killed_idx]
      donor_idx <- if (length(candidates) == 1L) {
        candidates
      } else {
        sample(candidates, 1L)
      }

      killed_pos <- x[killed_idx]

      pos_segs <- end_and_append_segment(
        pos_segs, col_map[killed_idx], row,
        pre_value = killed_pos,
        post_value = x[donor_idx]
      )
      col_map[killed_idx] <- pos_segs$idx

      x[killed_idx] <- x[donor_idx]

      cli_progress_update()
    }

    for (i in seq_len(n_particles)) {
      pos_segs$cols[[col_map[i]]][row] <- x[i]
    }
  }

  if (n_events > 0L) cli_progress_done()

  list(
    positions = finalize_segments(pos_segs),
    times = times,
    is_event = is_event
  )
}

# Internal: generate Poisson inter-event times on [0, time_horizon].
# Returns inter-event times whose cumulative sum does not exceed
# time_horizon.
poisson_event_times <- function(time_horizon, rate) {
  expected <- time_horizon * rate
  buffer <- max(as.integer(ceiling(expected + 4 * sqrt(expected) + 4)), 10L)
  tau <- stats::rexp(buffer, rate = rate)
  cumulative_times <- cumsum(tau)
  while (cumulative_times[length(cumulative_times)] <= time_horizon) {
    tau <- c(tau, stats::rexp(buffer, rate = rate))
    cumulative_times <- cumsum(tau)
  }
  n_events <- sum(cumulative_times <= time_horizon)
  if (n_events == 0L) {
    return(numeric(0))
  }
  tau[seq_len(n_events)]
}
