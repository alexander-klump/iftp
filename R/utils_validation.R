# Internal validation helpers (used across package) -------------------------

check_positive_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) {
    abort(paste0("`", name, "` must be a single positive number."))
  }
}

check_non_negative_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    abort(paste0("`", name, "` must be a single non-negative number."))
  }
}

check_numeric_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    abort(paste0("`", name, "` must be a single number."))
  }
}

check_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != trunc(x)) {
    abort(paste0("`", name, "` must be a single positive integer."))
  }
}

validate_process <- function(process, name = "process") {
  if (!is.function(process)) {
    abort(paste0("`", name, "` must be a function with signature (x, dt)."))
  }
  if (length(formals(process)) < 2L) {
    abort(paste0(
      "`", name, "` must accept at least 2 arguments (x, dt)."
    ))
  }
  invisible(TRUE)
}

validate_cost <- function(cost) {
  if (!is.function(cost)) {
    abort("`cost` must be a function.")
  }
  test <- cost(c(-1, 0, 1))
  if (!is.numeric(test) || length(test) != 3L) {
    abort(
      "`cost` must map a numeric vector to a numeric vector of the same length."
    )
  }
  invisible(TRUE)
}

validate_init <- function(init) {
  if (!is.function(init)) {
    abort("`init` must be a function with signature (n).")
  }
  invisible(TRUE)
}


require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    abort(c(
      "Package {ggplot2} is required for plotting.",
      i = "Install it with: install.packages(\"ggplot2\")"
    ))
  }
}
