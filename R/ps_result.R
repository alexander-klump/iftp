#' Construct a particle system result
#'
#' Internal constructor for the `ps_result` S3 class, the unified return
#' type for [nbmp_simulate()] and [qtps_simulate()].
#'
#' @param type Character scalar: `"nbmp"` or `"qtps"`.
#' @param times Numeric vector of recorded time points (starting at 0).
#' @param is_event Logical vector of the same length as `times`. `TRUE` at
#'   event times (branching/selection for N-BMP, deletion for QTPS).
#' @param positions Matrix of particle positions (rows = time points,
#'   columns = segments). Columns grow with each event or jump-induced
#'   segment break. Non-`NA` values indicate live segments at that time.
#' @param params Named list of simulation parameters (at minimum
#'   `n_particles` and `time_horizon`).
#'
#' @return A `ps_result` object.
#'
#' @keywords internal
new_ps_result <- function(type, times, is_event, positions, params) {
  structure(
    list(
      type = type,
      times = times,
      is_event = is_event,
      positions = positions,
      params = params
    ),
    class = "ps_result"
  )
}

#' Print a particle system result
#'
#' @param x A `ps_result` object.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @export
print.ps_result <- function(x, ...) {
  label <- if (x$type == "nbmp") "N-BMP simulation" else "QTPS"
  n_events <- sum(x$is_event)
  n_total <- length(x$times)
  if (n_total > n_events + 1L) {
    cat(sprintf(
      "%s: %d particles, %d events, %d time points\n",
      label, x$params$n_particles, n_events, n_total
    ))
  } else {
    cat(sprintf(
      "%s: %d particles, %d events\n",
      label, x$params$n_particles, n_events
    ))
  }
  cat(sprintf("Time range: [0, %.4f]\n", max(x$times)))
  invisible(x)
}
