#' Test if an object is an IFTP result
#'
#' @param x Object to test.
#' @return `TRUE` if `x` inherits from `"iftp_result"`, `FALSE` otherwise.
#' @examples
#' is_iftp_result(iftp_anulova(dist_exponential(1),
#'   time_horizon = 1,
#'   time_steps = 5, n_particles = 50, seed = 1
#' ))
#' is_iftp_result("not a result")
#' @family type-checks
#' @export
is_iftp_result <- function(x) {
  inherits(x, "iftp_result")
}

#' Test if an object is a particle system result
#'
#' @param x Object to test.
#' @return `TRUE` if `x` inherits from `"ps_result"`, `FALSE` otherwise.
#' @examples
#' is_ps_result(nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 1))
#' is_ps_result("not a result")
#' @family type-checks
#' @export
is_ps_result <- function(x) {
  inherits(x, "ps_result")
}

#' Test if an object is an IFTP distribution
#'
#' @param x Object to test.
#' @return `TRUE` if `x` inherits from `"iftp_dist"`, `FALSE` otherwise.
#' @examples
#' is_iftp_dist(dist_exponential(1))
#' is_iftp_dist(pexp)
#' @family type-checks
#' @export
is_iftp_dist <- function(x) {
  inherits(x, "iftp_dist")
}

#' Test if an object is an IFTP process
#'
#' @param x Object to test.
#' @return `TRUE` if `x` inherits from `"iftp_process"`, `FALSE` otherwise.
#' @examples
#' is_iftp_process(process_bm())
#' is_iftp_process(rnorm)
#' @family type-checks
#' @export
is_iftp_process <- function(x) {
  inherits(x, "iftp_process")
}
