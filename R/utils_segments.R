# Segment-tracking helpers (shared by nbmp_loop and qtps_loop) ---------------

#' Pre-allocate a segment column tracker with dynamic growth.
#' Each segment is a column vector of length `total_rows`, initialised to NA.
#'
#' @param total_rows Number of rows per column.
#' @param initial_values Optional numeric vector. If provided, creates one
#'   column per element, each with that value at row 1. If `NULL` (default),
#'   creates a single empty column.
#' @param initial_capacity Initial list capacity. Defaults to
#'   `max(2 * length(initial_values), 16)`.
#' @noRd
init_segments <- function(total_rows, initial_values = NULL,
                          initial_capacity = NULL) {
  if (is.null(initial_values)) {
    n <- 1L
    if (is.null(initial_capacity)) initial_capacity <- 16L
    cols <- vector("list", initial_capacity)
    cols[[1L]] <- rep(NA_real_, total_rows)
  } else {
    n <- length(initial_values)
    if (is.null(initial_capacity)) {
      initial_capacity <- max(2L * n, 16L)
    }
    cols <- vector("list", initial_capacity)
    for (j in seq_len(n)) {
      cols[[j]] <- rep(NA_real_, total_rows)
      cols[[j]][1L] <- initial_values[j]
    }
  }
  list(cols = cols, idx = n, total_rows = total_rows)
}

#' End the segment at `col_idx` with `pre_value` at `row`, then
#' append a new segment with `post_value` at the same row.
#' Grows capacity if needed (doubles list size).
#' @noRd
end_and_append_segment <- function(segs, col_idx, row, pre_value, post_value) {
  segs$cols[[col_idx]][row] <- pre_value
  segs$idx <- segs$idx + 1L
  # Grow if at capacity
  if (segs$idx > length(segs$cols)) {
    new_capacity <- length(segs$cols) * 2L
    segs$cols <- c(segs$cols, vector("list", new_capacity - length(segs$cols)))
  }
  segs$cols[[segs$idx]] <- rep(NA_real_, segs$total_rows)
  segs$cols[[segs$idx]][row] <- post_value
  segs
}

#' Collapse used segment columns into a matrix.
#' @noRd
finalize_segments <- function(segs) {
  do.call(cbind, segs$cols[seq_len(segs$idx)])
}
