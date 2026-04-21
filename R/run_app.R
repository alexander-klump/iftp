#' Launch the iftp Shiny application
#'
#' Opens an interactive browser-based UI for running IFTP boundary
#' approximations and particle system simulations. Requires \pkg{shiny},
#' \pkg{bslib}, and \pkg{ggplot2}.
#'
#' @param ... Arguments passed to [shiny::shinyAppDir()] (e.g., `options`).
#'
#' @return A `shiny.appobj` (invisibly). When called interactively,
#'   launches the app in a browser.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
run_app <- function(...) {
  for (pkg in c("shiny", "bslib", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      abort(c(
        paste0("Package '", pkg, "' is required to run the app."),
        i = sprintf("Install it with: install.packages(\"%s\")", pkg)
      ))
    }
  }
  app_dir <- system.file("shiny", "app", package = "iftp")
  if (app_dir == "") {
    abort("Could not find Shiny app directory. Is the package installed?")
  }
  shiny::shinyAppDir(app_dir, ...)
}

#' Dispatch simulation based on type and parameters
#'
#' @param params Named list with `sim_type` and algorithm-specific parameters.
#' @return An `iftp_result` or `ps_result` object.
#' @keywords internal
#' @export
shiny_dispatch_simulation <- function(params) {
  sim_type <- params$sim_type
  init <- params$init %||% function(n) rep(0, n)
  path_res <- params$path_resolution
  cap_seed <- isTRUE(params$capture_seed)
  switch(sim_type,
    anulova = iftp_anulova(
      cdf = params$dist, time_horizon = params$time_horizon,
      time_steps = params$time_steps,
      n_particles = params$n_particles,
      process = params$process, cost = params$cost,
      init = init, capture_seed = cap_seed, seed = params$seed
    ),
    quantile = iftp_quantile(
      cdf = params$dist, time_horizon = params$time_horizon,
      time_steps = params$time_steps,
      remove_per_step = params$remove_per_step,
      process = params$process, cost = params$cost,
      init = init, method = params$method %||% "quantiles",
      capture_seed = cap_seed, seed = params$seed
    ),
    soft_killing = iftp_soft_killing(
      cdf = params$dist, time_horizon = params$time_horizon,
      killing_rate = params$killing_rate,
      time_steps = params$time_steps, n_particles = params$n_particles,
      process = params$process, cost = params$cost,
      init = init, capture_seed = cap_seed, seed = params$seed
    ),
    nbmp = nbmp_simulate(
      n_particles = params$n_particles,
      time_horizon = params$time_horizon,
      branching_rate = params$branching_rate,
      process = params$process, cost = params$cost,
      init = init,
      path_resolution = path_res,
      capture_seed = cap_seed, seed = params$seed
    ),
    qtps = qtps_simulate(
      n_particles = params$n_particles, cdf = params$dist,
      time_horizon = params$time_horizon,
      process = params$process, cost = params$cost,
      init = init, method = params$method %||% "quantiles",
      path_resolution = path_res,
      capture_seed = cap_seed, seed = params$seed
    ),
    abort(paste0("Unknown simulation type: ", sim_type))
  )
}

#' Generate reproducible R code snippet for a simulation
#'
#' @param params Named list with `sim_type` and code-generation strings.
#' @return A character string of executable R code.
#' @keywords internal
#' @export
shiny_generate_code <- function(params) {
  sim_type <- params$sim_type
  seed_str <- if (is.null(params$seed)) "NULL" else as.character(params$seed)

  # Build trailing args: init (optional), then seed (always last)
  # For particle systems (nbmp/qtps), track and path_resolution go between
  # init and seed to match the function signature order.
  init_arg <- if (!is.null(params$init_code)) {
    sprintf("  init = %s", params$init_code)
  }
  seed_arg <- sprintf("  seed = %s", seed_str)
  method_arg <- if (!is.null(params$method) && params$method != "quantiles") {
    sprintf("  method = \"%s\"", params$method)
  }

  # IFTP algorithms: init, method (if non-default), seed
  iftp_trailing <- paste(
    Filter(Negate(is.null), list(init_arg, method_arg, seed_arg)),
    collapse = ",\n"
  )

  # Particle systems: init, method (if non-default), path_resolution, seed
  if (sim_type %in% c("nbmp", "qtps")) {
    path_res_arg <- if (!is.null(params$path_resolution)) {
      sprintf("  path_resolution = %s", params$path_resolution)
    }
    ps_trailing <- paste(
      Filter(Negate(is.null), list(init_arg, method_arg, path_res_arg, seed_arg)),
      collapse = ",\n"
    )
  }

  body <- switch(sim_type,
    anulova = sprintf(
      paste0(
        "iftp_anulova(\n  cdf = %s,\n",
        "  time_horizon = %s, time_steps = %s, n_particles = %s,\n",
        "  process = %s, cost = %s,\n%s\n)"
      ),
      params$dist_code, params$time_horizon, params$time_steps,
      params$n_particles,
      params$process_code, params$cost_code, iftp_trailing
    ),
    quantile = sprintf(
      paste0(
        "iftp_quantile(\n  cdf = %s, time_horizon = %s,\n",
        "  time_steps = %s, remove_per_step = %s,\n",
        "  process = %s, cost = %s,\n%s\n)"
      ),
      params$dist_code, params$time_horizon, params$time_steps,
      params$remove_per_step,
      params$process_code, params$cost_code, iftp_trailing
    ),
    soft_killing = sprintf(
      paste0(
        "iftp_soft_killing(\n  cdf = %s, time_horizon = %s,\n",
        "  killing_rate = %s,\n",
        "  time_steps = %s, n_particles = %s,\n",
        "  process = %s, cost = %s,\n%s\n)"
      ),
      params$dist_code, params$time_horizon,
      params$killing_rate,
      params$time_steps, params$n_particles,
      params$process_code, params$cost_code, iftp_trailing
    ),
    nbmp = sprintf(
      paste0(
        "nbmp_simulate(\n  n_particles = %s,\n",
        "  time_horizon = %s, branching_rate = %s,\n",
        "  process = %s, cost = %s,\n%s\n)"
      ),
      params$n_particles, params$time_horizon, params$branching_rate,
      params$process_code, params$cost_code, ps_trailing
    ),
    qtps = sprintf(
      paste0(
        "qtps_simulate(\n  n_particles = %s,\n",
        "  cdf = %s, time_horizon = %s,\n",
        "  process = %s, cost = %s,\n%s\n)"
      ),
      params$n_particles, params$dist_code, params$time_horizon,
      params$process_code, params$cost_code, ps_trailing
    ),
    abort(paste0("Unknown simulation type: ", sim_type))
  )

  paste0("library(iftp)\n\nresult <- ", body, "\n")
}
