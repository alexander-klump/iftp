#' Plot an IFTP boundary curve
#'
#' Plots a boundary approximation over time. Requires the
#' \pkg{ggplot2} package.
#'
#' @param x An `iftp_result` object from [iftp_anulova()],
#'   [iftp_quantile()], or [iftp_soft_killing()].
#' @param title Plot title. Defaults to `"IFTP Boundary Approximation"`.
#' @param ... Additional arguments passed from the S3 method (currently unused).
#' @param reference_line Optional numeric value to draw as a horizontal
#'   reference line (e.g., the Shiryaev constant `pi / (2 * sqrt(2))`).
#' @param cost_transform Logical. If `TRUE` (default), boundary values are
#'   transformed by the cost function before plotting, giving the boundary
#'   in cost space. If `FALSE`, the raw particle positions at the survival
#'   threshold are shown.
#'
#' @return A `ggplot` object (invisibly).
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   b1 <- iftp_anulova(dist_exponential(),
#'     time_horizon = 2, time_steps = 50, n_particles = 500,
#'     seed = 42
#'   )
#'   plot_iftp_result(b1)
#' }
#'
#' @family iftp
#' @export
plot_iftp_result <- function(x,
                             title = "IFTP Boundary Approximation",
                             reference_line = NULL,
                             cost_transform = TRUE) {
  require_ggplot2()
  if (!inherits(x, "iftp_result")) {
    abort("Argument must be an `iftp_result` object.")
  }

  algorithm <- x$params$algorithm %||% "unknown"
  boundary <- x$boundary[!is.na(x$boundary$boundary), ]

  if (cost_transform && !is.null(x$params$cost)) {
    boundary$boundary <- x$params$cost(boundary$boundary)
  }

  use_points <- algorithm %in% c("anulova", "quantile")
  y_label <- if (cost_transform) "Cost boundary cost(b(t))" else "Boundary b(t)"

  p <- ggplot2::ggplot(boundary, ggplot2::aes(
    x = .data$time, y = .data$boundary
  ))

  if (use_points) {
    p <- p + ggplot2::geom_point(size = 1.5)
  } else {
    p <- p + ggplot2::geom_line(linewidth = 0.8)
  }

  p <- add_overlays(p, reference_line,
    iftp_boundary = NULL, title,
    y_label = y_label
  )
  print(p)
  invisible(p)
}

#' @rdname plot_iftp_result
#' @export
plot.iftp_result <- function(x, ...) {
  plot_iftp_result(x, ...)
}
