test_that("plot_ps_result works with nbmp result", {
  skip_if_not_installed("ggplot2")
  result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
  p <- plot_ps_result(result)
  expect_s3_class(p, "gg")
})

test_that("plot_ps_result works with qtps result", {
  skip_if_not_installed("ggplot2")
  result <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(), time_horizon = 1, seed = 42
  )
  p <- plot_ps_result(result)
  expect_s3_class(p, "gg")
})

test_that("plot_ps_result returns invisibly", {
  skip_if_not_installed("ggplot2")
  result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
  expect_invisible(plot_ps_result(result))
})

test_that("plot_ps_result accepts reference_line", {
  skip_if_not_installed("ggplot2")
  result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
  p <- plot_ps_result(result, reference_line = pi / (2 * sqrt(2)))
  expect_s3_class(p, "gg")
})

test_that("plot_ps_result accepts iftp_boundary overlay", {
  skip_if_not_installed("ggplot2")
  result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
  boundary <- data.frame(time = c(0, 0.25, 0.5), boundary = c(0, 0.8, 1.0))
  p <- plot_ps_result(result, iftp_boundary = boundary)
  expect_s3_class(p, "gg")
})

test_that("plot_ps_result validates input", {
  expect_error(plot_ps_result("not_a_result"), "ps_result")
})

test_that("plot_ps_result accepts iftp_result as iftp_boundary", {
  skip_if_not_installed("ggplot2")
  result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
  boundary <- iftp_anulova(dist_exponential(),
    time_horizon = 0.5,
    time_steps = 10, n_particles = 100, seed = 1
  )
  p <- plot_ps_result(result, iftp_boundary = boundary)
  expect_s3_class(p, "gg")
})

test_that("plot() S3 method dispatches for ps_result", {
  skip_if_not_installed("ggplot2")
  result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
  p <- plot(result)
  expect_s3_class(p, "gg")
})

test_that("plot_ps_result cost_transform affects iftp_boundary overlay", {
  skip_if_not_installed("ggplot2")
  ps <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
  iftp <- iftp_anulova(dist_exponential(),
    time_horizon = 0.5,
    time_steps = 10, n_particles = 100, cost = abs, seed = 1
  )
  p_cost <- plot_ps_result(ps, iftp_boundary = iftp, cost_transform = TRUE)
  p_raw <- plot_ps_result(ps, iftp_boundary = iftp, cost_transform = FALSE)
  layers_cost <- ggplot2::ggplot_build(p_cost)$data
  layers_raw <- ggplot2::ggplot_build(p_raw)$data
  # The IFTP boundary overlay is the second data layer (after particle paths)
  boundary_cost <- layers_cost[[2]]$y
  boundary_raw <- layers_raw[[2]]$y
  expect_false(identical(boundary_cost, boundary_raw))
})
