test_that("shiny_dispatch_simulation runs anulova", {
  params <- list(
    sim_type = "anulova",
    dist = dist_exponential(),
    process = process_bm(),
    cost = abs,
    seed = 42,
    time_horizon = 1, time_steps = 20, n_particles = 100
  )
  result <- shiny_dispatch_simulation(params)
  expect_s3_class(result, "iftp_result")
  expect_true(nrow(result$boundary) > 0L)
})

test_that("shiny_dispatch_simulation runs quantile", {
  params <- list(
    sim_type = "quantile",
    dist = dist_exponential(),
    process = process_bm(),
    cost = abs,
    seed = 42,
    time_horizon = 1, time_steps = 6, remove_per_step = 1
  )
  result <- shiny_dispatch_simulation(params)
  expect_s3_class(result, "iftp_result")
})

test_that("shiny_dispatch_simulation runs soft_killing", {
  params <- list(
    sim_type = "soft_killing",
    dist = dist_exponential(),
    process = process_bm(),
    cost = function(x) -x,
    seed = 42,
    time_horizon = 1, killing_rate = 1, time_steps = 3, n_particles = 200
  )
  result <- shiny_dispatch_simulation(params)
  expect_s3_class(result, "iftp_result")
})

test_that("shiny_dispatch_simulation runs nbmp", {
  params <- list(
    sim_type = "nbmp",
    process = process_bm(),
    cost = identity,
    seed = 42,
    n_particles = 20, time_horizon = 0.5, branching_rate = 1
  )
  result <- shiny_dispatch_simulation(params)
  expect_s3_class(result, "ps_result")
})

test_that("shiny_dispatch_simulation runs qtps", {
  params <- list(
    sim_type = "qtps",
    dist = dist_exponential(),
    process = process_bm(),
    cost = abs,
    seed = 42,
    n_particles = 20, time_horizon = 1
  )
  result <- shiny_dispatch_simulation(params)
  expect_s3_class(result, "ps_result")
})

test_that("shiny_dispatch_simulation errors on unknown type", {
  expect_error(shiny_dispatch_simulation(list(sim_type = "bogus")), "Unknown")
})

test_that("shiny_generate_code produces valid anulova code", {
  params <- list(
    sim_type = "anulova",
    dist_code = "dist_exponential(rate = 1)",
    process_code = "process_bm(sigma = 1)",
    cost_code = "abs",
    seed = 42,
    time_horizon = 2, time_steps = 100, n_particles = 1000
  )
  code <- shiny_generate_code(params)
  expect_true(grepl("library(iftp)", code, fixed = TRUE))
  expect_true(grepl("iftp_anulova", code, fixed = TRUE))
  expect_true(grepl("dist_exponential", code, fixed = TRUE))
  expect_true(grepl("seed = 42", code, fixed = TRUE))
})

test_that("shiny_generate_code produces valid nbmp code", {
  params <- list(
    sim_type = "nbmp",
    process_code = "process_bm(sigma = 1)",
    cost_code = "identity",
    seed = 7,
    n_particles = 50, time_horizon = 1, branching_rate = 1
  )
  code <- shiny_generate_code(params)
  expect_true(grepl("nbmp_simulate", code, fixed = TRUE))
})

test_that("shiny_generate_code produces valid quantile code", {
  params <- list(
    sim_type = "quantile",
    dist_code = "dist_weibull(shape = 2, scale = 1)",
    process_code = "process_bm(sigma = 1)",
    cost_code = "abs",
    seed = 1,
    time_steps = 8, remove_per_step = 1, time_horizon = 2
  )
  code <- shiny_generate_code(params)
  expect_true(grepl("iftp_quantile", code, fixed = TRUE))
  expect_true(grepl("time_steps = 8", code, fixed = TRUE))
  expect_true(grepl("remove_per_step = 1", code, fixed = TRUE))
})

test_that("shiny_generate_code produces valid soft_killing code", {
  params <- list(
    sim_type = "soft_killing",
    dist_code = "dist_exponential(rate = 1)",
    process_code = "process_bm(sigma = 1)",
    cost_code = "function(x) -x",
    seed = 5,
    time_horizon = 2, killing_rate = 1, time_steps = 5, n_particles = 1000
  )
  code <- shiny_generate_code(params)
  expect_true(grepl("iftp_soft_killing", code, fixed = TRUE))
  expect_true(grepl("killing_rate = 1", code, fixed = TRUE))
})

test_that("shiny_generate_code produces valid qtps code", {
  params <- list(
    sim_type = "qtps",
    dist_code = "dist_exponential(rate = 1)",
    process_code = "process_bm(sigma = 1)",
    cost_code = "abs",
    seed = 3,
    n_particles = 50, time_horizon = 2
  )
  code <- shiny_generate_code(params)
  expect_true(grepl("qtps_simulate", code, fixed = TRUE))
})

test_that("shiny_generate_code errors on unknown type", {
  params <- list(sim_type = "bogus", seed = NULL)
  expect_error(shiny_generate_code(params), "Unknown")
})

test_that("shiny_generate_code handles function(x) -x cost code", {
  params <- list(
    sim_type = "soft_killing",
    dist_code = "dist_exponential(rate = 1)",
    process_code = "process_bm(sigma = 1)",
    cost_code = "function(x) -x",
    seed = 42,
    time_horizon = 2, killing_rate = 1, time_steps = 5, n_particles = 1000
  )
  code <- shiny_generate_code(params)
  expect_true(grepl("cost = function(x) -x", code, fixed = TRUE))
  expect_false(grepl("cost = neg", code, fixed = TRUE))
})

test_that("shiny_generate_code handles NULL seed", {
  params <- list(
    sim_type = "anulova",
    dist_code = "dist_exponential(rate = 1)",
    process_code = "process_bm(sigma = 1)",
    cost_code = "abs",
    seed = NULL,
    time_horizon = 2, time_steps = 100, n_particles = 1000
  )
  code <- shiny_generate_code(params)
  expect_true(grepl("seed = NULL", code, fixed = TRUE))
})

test_that("shiny_generate_code produces executable anulova code", {
  params <- list(
    sim_type = "anulova",
    dist_code = "dist_exponential(rate = 1)",
    process_code = "process_bm(sigma = 1)",
    cost_code = "abs",
    seed = 42,
    time_horizon = 1, time_steps = 20, n_particles = 100
  )
  code <- shiny_generate_code(params)
  code <- sub("library(iftp)\n\n", "", code, fixed = TRUE)
  env <- new.env(parent = asNamespace("iftp"))
  eval(parse(text = code), envir = env)
  expect_s3_class(env$result, "iftp_result")
})

test_that("shiny_generate_code produces executable nbmp code", {
  params <- list(
    sim_type = "nbmp",
    process_code = "process_bm(sigma = 1)",
    cost_code = "identity",
    init_code = "function(n) runif(n, -1, 1)",
    path_resolution = 5,
    seed = 42,
    n_particles = 20, time_horizon = 0.5, branching_rate = 1
  )
  code <- shiny_generate_code(params)
  code <- sub("library(iftp)\n\n", "", code, fixed = TRUE)
  env <- new.env(parent = asNamespace("iftp"))
  eval(parse(text = code), envir = env)
  expect_s3_class(env$result, "ps_result")
})

test_that("app helper functions are exported", {
  exports <- getNamespaceExports("iftp")
  expect_true("shiny_dispatch_simulation" %in% exports)
  expect_true("export_result" %in% exports)
  expect_true("shiny_generate_code" %in% exports)
})

test_that("run_app returns a shiny app object", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("ggplot2")
  app_dir <- system.file("shiny", "app", package = "iftp")
  skip_if(app_dir == "", "Shiny app directory not available")
  skip_if(
    !file.exists(file.path(app_dir, "app.R")),
    "Shiny app.R not yet created"
  )
  app <- run_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("run_app is a function", {
  expect_true(is.function(run_app))
})

test_that("shiny_dispatch_simulation forwards init parameter", {
  default <- shiny_dispatch_simulation(list(
    sim_type = "anulova", dist = dist_exponential(),
    process = process_bm(), cost = abs, seed = 42,
    time_horizon = 1, time_steps = 20, n_particles = 100
  ))
  shifted <- shiny_dispatch_simulation(list(
    sim_type = "anulova", dist = dist_exponential(),
    process = process_bm(), cost = abs, seed = 42,
    init = function(n) rep(5, n),
    time_horizon = 1, time_steps = 20, n_particles = 100
  ))
  expect_s3_class(shifted, "iftp_result")
  expect_false(
    identical(default$boundary$boundary, shifted$boundary$boundary),
    "init = rep(5, n) should produce a different boundary than default"
  )
})

test_that("shiny_dispatch_simulation forwards path_resolution for nbmp", {
  params <- list(
    sim_type = "nbmp",
    process = process_bm(),
    cost = identity, seed = 42,
    init = function(n) rep(0, n),
    path_resolution = 10,
    n_particles = 20, time_horizon = 0.5, branching_rate = 1
  )
  result <- shiny_dispatch_simulation(params)
  expect_s3_class(result, "ps_result")
})

test_that("shiny_dispatch_simulation forwards path_resolution for qtps", {
  params <- list(
    sim_type = "qtps",
    dist = dist_exponential(),
    process = process_bm(),
    cost = abs, seed = 42,
    init = function(n) rep(0, n),
    path_resolution = 5,
    n_particles = 20, time_horizon = 1
  )
  result <- shiny_dispatch_simulation(params)
  expect_s3_class(result, "ps_result")
})

test_that("shiny_generate_code includes init when non-default", {
  params <- list(
    sim_type = "anulova",
    dist_code = "dist_exponential(rate = 1)",
    process_code = "process_bm(sigma = 1)",
    cost_code = "abs",
    init_code = "function(n) runif(n, -1, 1)",
    seed = 42,
    time_horizon = 2, time_steps = 100, n_particles = 1000
  )
  code <- shiny_generate_code(params)
  expect_true(grepl("init = function(n) runif", code, fixed = TRUE))
})

test_that("shiny_generate_code includes path_resolution for nbmp", {
  params <- list(
    sim_type = "nbmp",
    process_code = "process_bm(sigma = 1)",
    cost_code = "identity",
    init_code = NULL,
    path_resolution = 10,
    seed = 42,
    n_particles = 50, time_horizon = 1, branching_rate = 1
  )
  code <- shiny_generate_code(params)
  expect_true(grepl("path_resolution = 10", code, fixed = TRUE))
})

test_that("shiny_generate_code omits path_resolution when NULL", {
  params <- list(
    sim_type = "nbmp",
    process_code = "process_bm(sigma = 1)",
    cost_code = "identity",
    init_code = NULL,
    path_resolution = NULL,
    seed = 42,
    n_particles = 50, time_horizon = 1, branching_rate = 1
  )
  code <- shiny_generate_code(params)
  expect_false(grepl("path_resolution", code, fixed = TRUE))
})
