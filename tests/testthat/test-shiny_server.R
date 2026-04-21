# Source the config module so config_server etc. are available
app_dir <- system.file("shiny", "app", package = "iftp")

skip_if(
  app_dir == "" || !file.exists(file.path(app_dir, "app.R")),
  "Shiny app not available"
)

for (f in list.files(file.path(app_dir, "R"),
  pattern = "\\.R$", full.names = TRUE
)) {
  source(f, local = TRUE)
}

test_that("config_server returns valid params for anulova", {
  shiny::testServer(config_server, {
    session$setInputs(
      sim_type = "anulova",
      dist_family = "exponential",
      dist_rate = 2,
      process_type = "bm",
      proc_sigma = 1,
      cost_type = "abs",
      init_type = "fixed",
      init_fixed_value = 0,
      path_resolution = NA,
      seed = 42,
      anulova_time_horizon = 1,
      anulova_time_steps = 50,
      anulova_n_particles = 200
    )
    params <- session$returned()
    expect_equal(params$sim_type, "anulova")
    expect_equal(params$time_horizon, 1)
    expect_equal(params$time_steps, 50)
    expect_equal(params$n_particles, 200)
    expect_true(is.function(params$cost))
    expect_true(is.function(params$process))
    expect_true(inherits(params$dist, "iftp_dist"))
    expect_equal(params$seed, 42)
    expect_equal(params$cost_code, "abs")
  })
})

test_that("config_server returns valid params for nbmp", {
  shiny::testServer(config_server, {
    session$setInputs(
      sim_type = "nbmp",
      process_type = "bm",
      proc_sigma = 1,
      cost_type = "identity",
      init_type = "uniform",
      init_uniform_a = -1,
      init_uniform_b = 1,
      path_resolution = 10,
      seed = NA,
      nbmp_n_particles = 30,
      nbmp_time_horizon = 0.5,
      nbmp_branching_rate = 1
    )
    params <- session$returned()
    expect_equal(params$sim_type, "nbmp")
    expect_equal(params$n_particles, 30)
    expect_equal(params$path_resolution, 10)
    expect_null(params$seed)
    expect_true(is.function(params$init))
    expect_equal(params$init_code, "function(n) runif(n, -1, 1)")
  })
})

test_that("config_server maps neg cost_code correctly", {
  shiny::testServer(config_server, {
    session$setInputs(
      sim_type = "soft_killing",
      dist_family = "exponential",
      dist_rate = 1,
      process_type = "bm",
      proc_sigma = 1,
      cost_type = "neg",
      init_type = "fixed",
      init_fixed_value = 0,
      path_resolution = NA,
      seed = NA,
      soft_killing_time_horizon = 2,
      soft_killing_killing_rate = 1,
      soft_killing_time_steps = 5,
      soft_killing_n_particles = 500
    )
    params <- session$returned()
    expect_equal(params$cost_code, "function(x) -x")
  })
})

test_that("config_server handles all distribution families", {
  for (family in names(DIST_CHOICES)) {
    dist_key <- DIST_CHOICES[[family]]
    shiny::testServer(config_server, {
      inputs <- list(
        sim_type = "anulova",
        dist_family = dist_key,
        process_type = "bm",
        proc_sigma = 1,
        cost_type = "abs",
        init_type = "fixed",
        init_fixed_value = 0,
        path_resolution = NA,
        seed = NA,
        anulova_time_horizon = 1, anulova_time_steps = 10, anulova_n_particles = 50
      )
      # Add dist-specific params with defaults
      dist_params <- DIST_PARAMS[[dist_key]]
      for (pname in names(dist_params)) {
        inputs[[paste0("dist_", pname)]] <- dist_params[[pname]]$value
      }
      do.call(session$setInputs, inputs)
      params <- session$returned()
      expect_true(
        inherits(params$dist, "iftp_dist"),
        info = paste("Failed for distribution:", dist_key)
      )
    })
  }
})

# -- Integration: config → dispatch → result ----------------------------------

test_that("full pipeline: anulova config → dispatch → iftp_result", {
  shiny::testServer(config_server, {
    session$setInputs(
      sim_type = "anulova",
      dist_family = "exponential",
      dist_rate = 1,
      process_type = "bm",
      proc_sigma = 1,
      cost_type = "abs",
      init_type = "fixed",
      init_fixed_value = 0,
      path_resolution = NA,
      seed = 42,
      anulova_time_horizon = 1,
      anulova_time_steps = 20,
      anulova_n_particles = 100
    )
    params <- session$returned()
    result <- iftp::shiny_dispatch_simulation(params)
    expect_s3_class(result, "iftp_result")
    expect_true(nrow(result$boundary) > 0L)
    # Verify export_result works on this result
    b <- iftp::export_result(result)
    expect_true(all(c("time", "boundary") %in% names(b)))
    # Verify generate_code produces runnable code
    code <- iftp::shiny_generate_code(params)
    expect_true(grepl("iftp_anulova", code, fixed = TRUE))
  })
})

test_that("full pipeline: nbmp config → dispatch → ps_result", {
  shiny::testServer(config_server, {
    session$setInputs(
      sim_type = "nbmp",
      process_type = "bm",
      proc_sigma = 1,
      cost_type = "identity",
      init_type = "uniform",
      path_resolution = 5,
      seed = 7,
      nbmp_n_particles = 20,
      nbmp_time_horizon = 0.5,
      nbmp_branching_rate = 1
    )
    params <- session$returned()
    result <- iftp::shiny_dispatch_simulation(params)
    expect_s3_class(result, "ps_result")
    expect_true(!is.null(result$positions))
    expect_true(is.matrix(result$positions))
    code <- iftp::shiny_generate_code(params)
    expect_true(grepl("nbmp_simulate", code, fixed = TRUE))
    expect_true(grepl("path_resolution = 5", code, fixed = TRUE))
  })
})

test_that("full pipeline: qtps config → dispatch → ps_result", {
  shiny::testServer(config_server, {
    session$setInputs(
      sim_type = "qtps",
      dist_family = "weibull",
      dist_shape = 2,
      dist_scale = 1,
      process_type = "bm",
      proc_sigma = 1,
      cost_type = "abs",
      init_type = "normal",
      init_normal_mu = 0,
      init_normal_sigma = 0.1,
      path_resolution = NA,
      seed = 3,
      qtps_n_particles = 20,
      qtps_time_horizon = 1
    )
    params <- session$returned()
    result <- iftp::shiny_dispatch_simulation(params)
    expect_s3_class(result, "ps_result")
    expect_true(is.matrix(result$positions))
    code <- iftp::shiny_generate_code(params)
    expect_true(grepl("qtps_simulate", code, fixed = TRUE))
    expect_true(grepl("init = function(n) rnorm", code, fixed = TRUE))
  })
})

test_that("dispatch passes method to iftp_quantile", {
  params <- list(
    sim_type = "quantile",
    dist = dist_exponential(),
    time_horizon = 2, time_steps = 10, remove_per_step = 10,
    process = process_bm(), cost = abs, init = NULL,
    method = "order_statistics", seed = 42
  )
  result <- shiny_dispatch_simulation(params)
  expect_s3_class(result, "iftp_result")
  expect_equal(result$params$method, "order_statistics")
})

test_that("dispatch passes method to qtps_simulate", {
  params <- list(
    sim_type = "qtps",
    dist = dist_exponential(),
    n_particles = 30, time_horizon = 2,
    process = process_bm(), cost = abs, init = NULL,
    path_resolution = NULL,
    method = "order_statistics", seed = 42
  )
  result <- shiny_dispatch_simulation(params)
  expect_s3_class(result, "ps_result")
  expect_equal(result$params$method, "order_statistics")
})

test_that("codegen includes method for quantile", {
  params <- list(
    sim_type = "quantile",
    dist_code = "dist_exponential()",
    time_horizon = 2, time_steps = 10, remove_per_step = 10,
    process_code = "process_bm()", cost_code = "abs",
    init_code = NULL, seed = NULL,
    method = "order_statistics"
  )
  code <- shiny_generate_code(params)
  expect_match(code, "method = \"order_statistics\"")
})

test_that("codegen includes method for qtps", {
  params <- list(
    sim_type = "qtps",
    dist_code = "dist_exponential()",
    n_particles = 30, time_horizon = 2,
    process_code = "process_bm()", cost_code = "abs",
    init_code = NULL, path_resolution = NULL, seed = NULL,
    method = "order_statistics"
  )
  code <- shiny_generate_code(params)
  expect_match(code, "method = \"order_statistics\"")
})

test_that("codegen omits method when quantiles (default)", {
  params <- list(
    sim_type = "quantile",
    dist_code = "dist_exponential()",
    time_horizon = 2, time_steps = 10, remove_per_step = 10,
    process_code = "process_bm()", cost_code = "abs",
    init_code = NULL, seed = NULL,
    method = "quantiles"
  )
  code <- shiny_generate_code(params)
  expect_no_match(code, "method")
})
