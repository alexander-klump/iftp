#' Export simulation result
#'
#' Exports a simulation result to CSV, JSON, or returns it as an R object.
#'
#' For `iftp_result`, always returns the boundary data frame (columns:
#' `time`, `boundary`).
#'
#' For `ps_result`, the output depends on the file type and `format`:
#'
#' - **CSV** (or `file = NULL` without `format`): flat data frame with
#'   columns `particle_id`, `time`, `position`, `is_alive`. One row per
#'   (segment, time point) pair, including rows where the particle is dead
#'   (`is_alive = FALSE`, `position` is `NA`).
#'
#' - **JSON with `format = "matrix"`**: list with elements `times` (numeric
#'   vector) and `positions` (list of numeric vectors, one per time point).
#'   Direct serialization of the positions matrix; `NA` values become `null`.
#'
#' - **JSON with `format = "segments"`**: list of per-segment objects, each
#'   with `id` (integer), `times` (numeric vector), and `positions` (numeric
#'   vector) containing only the non-`NA` entries for that segment column.
#'
#' @param result An `iftp_result` or `ps_result` object.
#' @param file Output file path. Use `.csv` or `.json` extension to select
#'   file type. If `NULL` (default), returns the data without writing to
#'   disk.
#' @param format JSON format for `ps_result` objects: `"matrix"` or
#'   `"segments"`. Required when writing `ps_result` to JSON. Ignored for
#'   `iftp_result` and for CSV output.
#'
#' @return A data frame or list (invisibly if `file` is specified).
#'
#' @details
#' For N-BMP `ps_result`, the position matrix columns grow with each event
#' (branching) and each jump-induced segment break. Each column represents
#' a contiguous path segment between discontinuities.
#'
#' For QTPS `ps_result`, the position matrix has exactly `n_particles`
#' columns. Deleted particles become `NA` from the deletion step onward.
#'
#' JSON export requires the \pkg{jsonlite} package.
#'
#' @examples
#' # IFTP result
#' b <- iftp_anulova(dist_exponential(),
#'   time_horizon = 1, time_steps = 10,
#'   n_particles = 100, seed = 1
#' )
#' head(export_result(b))
#'
#' # Particle system — CSV / data frame (default)
#' result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
#' head(export_result(result))
#'
#' # Particle system — matrix format
#' mat <- export_result(result, format = "matrix")
#' str(mat) # list with "times" and "positions"
#'
#' # Particle system — segments format
#' segs <- export_result(result, format = "segments")
#' str(segs[[1]]) # list with "id", "times", "positions"
#'
#' @family result-io
#' @export
export_result <- function(result, file = NULL, format = NULL) {
  if (!inherits(result, "iftp_result") && !inherits(result, "ps_result")) {
    abort("`result` must be an `iftp_result` or `ps_result` object.")
  }

  if (inherits(result, "iftp_result")) {
    return(write_result_file(result$boundary, file))
  }

  # ps_result
  if (!is.null(format)) {
    format <- match.arg(format, c("matrix", "segments"))
  }

  if (is.null(format)) {
    # No format: CSV or file=NULL → long data frame
    if (is_json_file(file)) {
      abort(c(
        "JSON export of `ps_result` requires `format`.",
        i = "Use `format = \"matrix\"` or `format = \"segments\"`."
      ))
    }
    data <- ps_to_long(result)
  } else {
    validate_json_format(file, format)
    data <- if (format == "matrix") {
      ps_to_matrix_list(result)
    } else {
      ps_to_segments_list(result)
    }
  }

  write_result_file(data, file)
}

# Internal: TRUE if file has .json extension.
is_json_file <- function(file) {
  !is.null(file) && tolower(tools::file_ext(file)) == "json"
}

# Internal: abort if file is non-NULL and not .json.
validate_json_format <- function(file, format) {
  if (!is.null(file) && !is_json_file(file)) {
    abort(c(
      sprintf("Format \"%s\" requires JSON output.", format),
      i = "Use a `.json` file extension, or `file = NULL` to return the list."
    ))
  }
}

# Internal: convert ps_result positions matrix to long-format data frame.
ps_to_long <- function(result) {
  n_cols <- ncol(result$positions)
  n_times <- length(result$times)
  pos_vec <- as.vector(result$positions)

  data.frame(
    particle_id = rep(seq_len(n_cols), each = n_times),
    time = rep(result$times, times = n_cols),
    position = pos_vec,
    is_alive = !is.na(pos_vec)
  )
}

# Internal: convert ps_result to matrix-format list.
# Returns list(times = numeric, positions = list of numeric vectors).
ps_to_matrix_list <- function(result) {
  n_rows <- nrow(result$positions)
  rows <- vector("list", n_rows)
  for (i in seq_len(n_rows)) {
    rows[[i]] <- as.numeric(result$positions[i, ])
  }
  list(times = result$times, positions = rows)
}

# Internal: convert ps_result to segments-format list.
# Each segment is the non-NA span of one column. Returns a list of lists,
# each with elements: id (integer), times (numeric), positions (numeric).
ps_to_segments_list <- function(result) {
  n_cols <- ncol(result$positions)
  segments <- vector("list", n_cols)
  for (j in seq_len(n_cols)) {
    col <- result$positions[, j]
    alive <- which(!is.na(col))
    if (length(alive) == 0L) {
      segments[[j]] <- list(id = j, times = numeric(0), positions = numeric(0))
    } else {
      segments[[j]] <- list(
        id = j,
        times = result$times[alive],
        positions = col[alive]
      )
    }
  }
  segments
}

# Internal: write data frame or list to file (CSV/JSON) or return it.
write_result_file <- function(data, file) {
  if (!is.null(file)) {
    ext <- tolower(tools::file_ext(file))
    if (ext == "csv") {
      if (!is.data.frame(data)) {
        abort("CSV export requires a data frame. Use `format = \"long\"` or a `.json` file.")
      }
      utils::write.csv(data, file, row.names = FALSE)
    } else if (ext == "json") {
      require_jsonlite()
      jsonlite::write_json(data, file, auto_unbox = TRUE, na = "null")
    } else {
      abort("File extension must be `.csv` or `.json`.")
    }
    invisible(data)
  } else {
    data
  }
}

# Internal: check jsonlite availability.
require_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    abort(c(
      "Package {jsonlite} is required for JSON export.",
      i = "Install it with: install.packages(\"jsonlite\")"
    ))
  }
}

#' Generate a descriptive filename for a simulation result
#'
#' Builds a filename from the simulation parameters stored in the result
#' object, appending a timestamp to ensure uniqueness.
#'
#' @param result An `iftp_result` or `ps_result` object.
#' @param ext File extension (without dot), e.g. `"csv"`, `"json"`, `"rds"`.
#' @param suffix Optional suffix appended before the timestamp (e.g.
#'   `"matrix"` or `"segments"` for JSON export variants).
#'
#' @return A character string encoding all simulation parameters, e.g.
#'   `"anulova_exponential_n100_t1_steps20_ou_s42_YYYYMMDD_HHMMSS.csv"`.
#'   Includes process type (unless default BM), cost function (unless
#'   default for the algorithm), seed (when stored in params),
#'   path resolution, and grid method (when non-default).
#'
#' @examples
#' b <- iftp_anulova(dist_exponential(),
#'   time_horizon = 1, time_steps = 20,
#'   n_particles = 100, seed = 1
#' )
#' suggest_filename(b, "csv")
#' suggest_filename(b, "json")
#'
#' r <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
#' suggest_filename(r, "csv")
#' suggest_filename(r, "json", suffix = "segments")
#'
#' @family result-io
#' @export
suggest_filename <- function(result, ext = "csv", suffix = NULL) {
  if (!inherits(result, "iftp_result") && !inherits(result, "ps_result")) {
    abort("`result` must be an `iftp_result` or `ps_result` object.")
  }

  p <- result$params

  parts <- if (inherits(result, "iftp_result")) {
    filename_parts_iftp(result)
  } else {
    filename_parts_ps(result)
  }

  parts <- c(parts, filename_parts_common(result))

  if (!is.null(suffix)) parts <- c(parts, suffix)
  parts <- c(parts, format(Sys.time(), "%Y%m%d_%H%M%S"))
  paste0(paste(parts, collapse = "_"), ".", ext)
}

# Internal: filename parts for iftp_result.
filename_parts_iftp <- function(result) {
  p <- result$params
  parts <- p$algorithm
  if (!is.null(p$distribution)) {
    parts <- c(parts, sanitize_for_filename(p$distribution))
  }
  if (!is.null(p$n_particles)) {
    parts <- c(parts, paste0("n", fmt_num(p$n_particles)))
  }
  parts <- c(parts, paste0("t", fmt_num(p$time_horizon)))
  parts <- c(parts, paste0("steps", fmt_num(p$time_steps)))
  if (identical(p$algorithm, "soft_killing") && !is.null(p$killing_rate)) {
    parts <- c(parts, paste0("kr", fmt_num(p$killing_rate)))
  }
  if (identical(p$algorithm, "quantile") && !is.null(p$remove_per_step)) {
    parts <- c(parts, paste0("rm", fmt_num(p$remove_per_step)))
  }
  if (identical(p$algorithm, "quantile") &&
    !is.null(p$method) && p$method != "quantiles") {
    parts <- c(parts, p$method)
  }
  parts
}

# Internal: filename parts for ps_result.
filename_parts_ps <- function(result) {
  p <- result$params
  parts <- result$type
  if (!is.null(p$distribution)) {
    parts <- c(parts, sanitize_for_filename(p$distribution))
  }
  parts <- c(parts, paste0("n", fmt_num(p$n_particles)))
  parts <- c(parts, paste0("t", fmt_num(p$time_horizon)))
  if (identical(result$type, "nbmp") && !is.null(p$branching_rate)) {
    parts <- c(parts, paste0("br", fmt_num(p$branching_rate)))
  }
  if (!is.null(p$method) && p$method != "quantiles") {
    parts <- c(parts, p$method)
  }
  if (!is.null(p$path_resolution)) {
    parts <- c(parts, paste0("res", fmt_num(p$path_resolution)))
  }
  parts
}

# Internal: filename parts common to both result types (process, cost, seed).
filename_parts_common <- function(result) {
  p <- result$params
  parts <- character(0)

  proc_name <- extract_process_name(p$process)
  if (!is.null(proc_name) && proc_name != "bm") {
    parts <- c(parts, proc_name)
  }

  cost_name <- extract_cost_name(p$cost)
  if (!is.null(cost_name) && !is_default_cost(result, cost_name)) {
    parts <- c(parts, paste0("cost-", cost_name))
  }

  if (!is.null(p$seed)) {
    parts <- c(parts, paste0("s", fmt_num(p$seed)))
  }
  parts
}

# Internal: check if a cost name is the default for this result type.
is_default_cost <- function(result, cost_name) {
  if (inherits(result, "iftp_result")) {
    if (identical(result$params$algorithm, "soft_killing")) {
      return(cost_name == "neg")
    }
    return(cost_name == "abs")
  }
  if (identical(result$type, "nbmp")) {
    return(cost_name == "identity")
  }
  cost_name == "abs"
}

# Internal: extract process type name from an iftp_process object.
extract_process_name <- function(process) {
  if (is.null(process)) {
    return(NULL)
  }
  ptype <- attr(process, "process_type")
  if (!is.null(ptype)) {
    return(ptype)
  }
  NULL
}

# Internal: extract a short name for a cost function.
extract_cost_name <- function(cost) {
  if (is.null(cost)) {
    return(NULL)
  }
  if (identical(cost, abs)) {
    return("abs")
  }
  if (identical(cost, identity)) {
    return("identity")
  }
  # Check for function(x) -x pattern
  body_str <- deparse(body(cost))
  if (length(body_str) == 1L && grepl("^-x$", trimws(body_str))) {
    return("neg")
  }
  NULL
}

# Internal: format a number compactly for use in filenames.
fmt_num <- function(x) {
  if (x == floor(x)) {
    return(as.character(as.integer(x)))
  }
  format(x, scientific = FALSE, drop0trailing = TRUE)
}

# Internal: sanitize a distribution name for use in filenames.
# Lowercases, replaces non-alphanumeric runs with hyphens, trims edges.
sanitize_for_filename <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-+|-+$", "", x)
}
