#' @keywords internal
new_iftp_result <- function(boundary_df, params) {
  structure(
    list(
      boundary = boundary_df,
      params = params
    ),
    class = "iftp_result"
  )
}

#' @export
print.iftp_result <- function(x, ...) {
  algorithm <- x$params$algorithm %||% "unknown"
  n_points <- nrow(x$boundary)
  t_max <- max(x$boundary$time)
  cat(
    sprintf("IFTP boundary (%s): %d time points\n", algorithm, n_points),
    sprintf("Time range: [0, %.4f]\n", t_max),
    sep = ""
  )
  invisible(x)
}
