test_that("plot_iftp_result requires ggplot2", {
  skip_if_not_installed("ggplot2")
  b <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  expect_invisible(plot_iftp_result(b))
})

test_that("plot_iftp_result accepts iftp_result objects", {
  skip_if_not_installed("ggplot2")
  b <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  expect_s3_class(b, "iftp_result")
  p <- plot_iftp_result(b)
  expect_s3_class(p, "ggplot")
})

test_that("plot_iftp_result rejects plain data frames", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(time = 0:5, boundary = seq(0, 1, length.out = 6))
  expect_error(plot_iftp_result(df), "iftp_result")
})


test_that("plot_iftp_result with reference line", {
  skip_if_not_installed("ggplot2")
  b <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  p <- plot_iftp_result(b, reference_line = pi / (2 * sqrt(2)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_iftp_result validates inputs", {
  expect_error(plot_iftp_result())
  expect_error(plot_iftp_result(data.frame(x = 1, y = 2)), "iftp_result")
})

test_that("plot_iftp_result uses geom_point for anulova", {
  skip_if_not_installed("ggplot2")
  b <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 20,
    n_particles = 100, seed = 1
  )
  p <- plot_iftp_result(b)
  layers <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomPoint" %in% layers)
  expect_false("GeomLine" %in% layers || "GeomPath" %in% layers)
})

test_that("plot_iftp_result uses geom_point for quantile", {
  skip_if_not_installed("ggplot2")
  b <- iftp_quantile(
    cdf = dist_exponential(), time_horizon = 1,
    time_steps = 100, remove_per_step = 1, seed = 1
  )
  p <- plot_iftp_result(b)
  layers <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomPoint" %in% layers)
  expect_false("GeomLine" %in% layers || "GeomPath" %in% layers)
})

test_that("plot_iftp_result uses geom_line for soft_killing", {
  skip_if_not_installed("ggplot2")
  b <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1,
    killing_rate = 1, time_steps = 3,
    n_particles = 100, seed = 1
  )
  p <- plot_iftp_result(b)
  layers <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomLine" %in% layers || "GeomPath" %in% layers)
})


test_that("plot() S3 method dispatches for iftp_result", {
  skip_if_not_installed("ggplot2")
  b <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  p <- plot(b)
  expect_s3_class(p, "ggplot")
})

test_that("plot_iftp_result cost_transform affects y-axis values", {
  skip_if_not_installed("ggplot2")
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 2, time_steps = 50,
    n_particles = 500, cost = abs, seed = 1
  )
  p_cost <- plot_iftp_result(result, cost_transform = TRUE)
  p_raw <- plot_iftp_result(result, cost_transform = FALSE)
  y_cost <- ggplot2::ggplot_build(p_cost)$data[[1]]$y
  y_raw <- ggplot2::ggplot_build(p_raw)$data[[1]]$y
  expect_false(identical(y_cost, y_raw))
})
