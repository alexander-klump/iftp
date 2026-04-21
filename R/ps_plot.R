#' Plot particle system simulation results
#'
#' Visualizes particle paths from an N-BMP or QTPS simulation.
#' Requires the \pkg{ggplot2} package.
#'
#' @param x A `ps_result` object from [nbmp_simulate()] or
#'   [qtps_simulate()].
#' @param reference_line Optional numeric value for a horizontal
#'   reference line (e.g., Shiryaev constant `pi / (2 * sqrt(2))`).
#' @param iftp_boundary Optional data frame with `time` and `boundary`
#'   columns or an `iftp_result` object for comparison overlay.
#' @param cost_transform Logical. If `TRUE` (default) and `iftp_boundary`
#'   is an `iftp_result`, the cost function is applied to boundary values
#'   before overlaying. If `FALSE`, raw positions are shown.
#' @param title Plot title. Defaults to an automatic title.
#' @param ... Additional arguments passed from the S3 method (currently unused).
#'
#' @return A `ggplot` object (invisibly).
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   result <- nbmp_simulate(n_particles = 20, time_horizon = 1, seed = 42)
#'   plot_ps_result(result)
#' }
#'
#' @family ps
#' @export
plot_ps_result <- function(x,
                           reference_line = NULL,
                           iftp_boundary = NULL,
                           cost_transform = TRUE,
                           title = NULL) {
  require_ggplot2()
  if (!inherits(x, "ps_result")) {
    abort("Argument must be a `ps_result` object.")
  }

  if (is.null(title)) {
    title <- if (x$type == "nbmp") {
      "N-BMP Simulation"
    } else {
      "Quantile Thinning Particle System"
    }
  }
  if (!is.null(iftp_boundary) && inherits(iftp_boundary, "iftp_result")) {
    cost_fn <- iftp_boundary$params$cost
    iftp_boundary <- iftp_boundary$boundary
    if (cost_transform && !is.null(cost_fn)) {
      iftp_boundary$boundary <- cost_fn(iftp_boundary$boundary)
    }
  }

  p <- ggplot2::ggplot()

  p <- add_paths(p, data.frame(time = x$times, x$positions),
    label = "Particles", linewidth = 0.3, alpha = 0.5
  )

  p <- p + ggplot2::scale_colour_manual(values = c("Particles" = "black"))

  p <- add_overlays(p, reference_line, iftp_boundary, title,
    y_label = "Position"
  )
  print(p)
  invisible(p)
}

#' @rdname plot_ps_result
#' @export
plot.ps_result <- function(x, ...) {
  plot_ps_result(x, ...)
}
