#' Save a simulation result to disk
#'
#' Saves an `iftp_result` or `ps_result` object as an `.rds` file with
#' metadata (timestamp, package version, and optional user-supplied fields).
#'
#' @param result An `iftp_result` or `ps_result` object.
#' @param path File path (should end in `.rds`).
#' @param metadata An optional named list of additional metadata to store
#'   (e.g., description, algorithm parameters).
#'
#' @return The `path`, invisibly.
#'
#' @examples
#' b <- iftp_anulova(dist_exponential(),
#'   time_horizon = 1, time_steps = 20,
#'   n_particles = 100, seed = 1
#' )
#' tmp <- tempfile(fileext = ".rds")
#' save_result(b, tmp, metadata = list(note = "test run"))
#' loaded <- load_result(tmp)
#' print(loaded)
#' file.remove(tmp)
#'
#' @family result-io
#' @export
save_result <- function(result, path, metadata = list()) {
  if (!inherits(result, "iftp_result") && !inherits(result, "ps_result")) {
    abort("`result` must be an `iftp_result` or `ps_result` object.")
  }

  wrapper <- list(
    result = result,
    metadata = c(
      metadata,
      list(
        timestamp = Sys.time(),
        package_version = as.character(utils::packageVersion("iftp"))
      )
    )
  )

  saveRDS(wrapper, file = path)
  invisible(path)
}

#' Load a simulation result from disk
#'
#' Reads an `.rds` file previously saved by [save_result()].
#'
#' @param path File path to the `.rds` file.
#'
#' @return The original `iftp_result` or `ps_result` object. Save metadata
#'   is attached as `attr(result, "save_metadata")`.
#'
#' @examples
#' b <- iftp_anulova(dist_exponential(),
#'   time_horizon = 1, time_steps = 20,
#'   n_particles = 100, seed = 1
#' )
#' tmp <- tempfile(fileext = ".rds")
#' save_result(b, tmp)
#' loaded <- load_result(tmp)
#' attr(loaded, "save_metadata")$timestamp
#' file.remove(tmp)
#'
#' @family result-io
#' @export
load_result <- function(path) {
  if (!file.exists(path)) {
    abort(paste0("File not found: ", path))
  }
  wrapper <- readRDS(path)
  if (!is.list(wrapper) || !all(c("result", "metadata") %in% names(wrapper))) {
    abort("File does not appear to be a saved result (expected `result` and `metadata` elements).")
  }
  result <- wrapper$result
  attr(result, "save_metadata") <- wrapper$metadata
  result
}
