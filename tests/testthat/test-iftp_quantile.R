test_that("iftp_quantile returns iftp_result class", {
  result <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2, seed = 1
  )
  expect_s3_class(result, "iftp_result")
  expect_s3_class(result$boundary, "data.frame")
  expect_named(result$boundary, c("time", "boundary"))
  expect_equal(result$params$algorithm, "quantile")
  expect_true("cost" %in% names(result$params))
})

test_that("iftp_quantile returns correct structure", {
  result <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2, seed = 1
  )
  expect_equal(result$boundary$time[1], 0)
  expect_equal(result$boundary$boundary[1], 0)
})

test_that("iftp_quantile is reproducible with seed", {
  r1 <- iftp_quantile(
    time_steps = 10, remove_per_step = 10, cdf = dist_exponential(),
    time_horizon = 2, seed = 42
  )
  r2 <- iftp_quantile(
    time_steps = 10, remove_per_step = 10, cdf = dist_exponential(),
    time_horizon = 2, seed = 42
  )
  expect_equal(r1, r2)
})

test_that("iftp_quantile produces non-trivial boundary", {
  result <- iftp_quantile(
    time_steps = 20, remove_per_step = 20,
    cdf = dist_exponential(),
    time_horizon = 2, cost = abs, seed = 1
  )
  expect_true(any(result$boundary$boundary[-1] != 0))
})

test_that("iftp_quantile works with cost = identity", {
  result <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2, cost = identity, seed = 1
  )
  expect_s3_class(result, "iftp_result")
})

test_that("iftp_quantile cost = abs and cost = identity differ", {
  r_abs <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2, cost = abs, seed = 42
  )
  r_id <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2, cost = identity, seed = 42
  )
  expect_false(identical(r_abs$boundary$boundary, r_id$boundary$boundary))
})

test_that("iftp_quantile time grid is non-equidistant", {
  result <- iftp_quantile(
    time_steps = 20, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 3, seed = 1
  )
  diffs <- diff(result$boundary$time)
  expect_false(all(abs(diffs - diffs[1]) < 1e-10))
})

test_that("iftp_quantile accepts explicit qf", {
  result <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = function(x) pexp(x, rate = 1),
    qf = function(p) qexp(p, rate = 1),
    time_horizon = 2, seed = 1
  )
  expect_s3_class(result, "iftp_result")
})

test_that("iftp_quantile validates inputs", {
  expect_error(iftp_quantile(
    time_steps = 10, remove_per_step = 10, cdf = "bad",
    time_horizon = 2
  ))
  expect_error(iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(), time_horizon = -1
  ))
  expect_error(iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = function(x) pexp(x), time_horizon = 2
  ))
  expect_error(iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(), time_horizon = 2,
    cost = "abs"
  ))
})

test_that("iftp_quantile two-sided converges toward Shiryaev constant", {
  shiryaev <- pi / (2 * sqrt(2))
  result <- iftp_quantile(
    time_steps = 100, remove_per_step = 50,
    cdf = dist_exponential(),
    time_horizon = 5, cost = abs, seed = 123
  )
  late_boundary <- abs(tail(result$boundary$boundary, 20))
  expect_true(mean(late_boundary) > shiryaev * 0.7)
  expect_true(mean(late_boundary) < shiryaev * 1.5)
})

test_that("iftp_quantile warns on particle depletion", {
  expect_warning(
    iftp_quantile(
      cdf = dist_uniform(0, 1), time_horizon = 2, time_steps = 3,
      remove_per_step = 2, seed = 42
    ),
    "Insufficient"
  )
})

test_that("iftp_quantile custom cost function works", {
  result <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2, cost = function(x) x^2, seed = 1
  )
  expect_s3_class(result, "iftp_result")
})

test_that("iftp_quantile custom init shifts boundary", {
  r_default <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2, seed = 42
  )
  r_shifted <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2,
    init = function(n) rep(1, n), seed = 42
  )
  expect_s3_class(r_shifted, "iftp_result")
  expect_false(identical(r_default$boundary$boundary, r_shifted$boundary$boundary))
})

test_that("iftp_quantile works with BM+Poisson process", {
  result <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2,
    process = process_bm_drift_poisson(), cost = identity,
    seed = 1
  )
  expect_s3_class(result, "iftp_result")
  expect_true(nrow(result$boundary) > 1)
})

test_that("iftp_quantile default cost is abs", {
  result <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(),
    time_horizon = 2, seed = 1
  )
  expect_identical(result$params$cost, abs)
})

test_that("iftp_quantile aborts on NaN from process", {
  bad_process <- function(x, dt) rep(NaN, length(x))
  class(bad_process) <- "iftp_process"
  attr(bad_process, "process_type") <- "bad"
  expect_error(
    iftp_quantile(
      time_steps = 10, remove_per_step = 10,
      cdf = dist_exponential(), time_horizon = 2,
      process = bad_process, seed = 1
    ),
    "non-finite"
  )
})

test_that("iftp_quantile method = quantiles is default (unchanged behavior)", {
  r1 <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  r2 <- iftp_quantile(
    time_steps = 10, remove_per_step = 10,
    cdf = dist_exponential(), time_horizon = 2,
    method = "quantiles", seed = 42
  )
  expect_equal(r1, r2)
})

test_that("iftp_quantile method = order_statistics returns iftp_result", {
  expect_warning(
    result <- iftp_quantile(
      time_steps = 10, remove_per_step = 10,
      cdf = dist_exponential(), time_horizon = 2,
      method = "order_statistics", seed = 42
    ),
    "Insufficient particles"
  )
  expect_s3_class(result, "iftp_result")
  expect_s3_class(result$boundary, "data.frame")
  expect_named(result$boundary, c("time", "boundary"))
  expect_equal(result$params$method, "order_statistics")
})

test_that("iftp_quantile order_statistics is reproducible with seed", {
  suppressWarnings({
    r1 <- iftp_quantile(
      time_steps = 10, remove_per_step = 10,
      cdf = dist_exponential(), time_horizon = 2,
      method = "order_statistics", seed = 42
    )
    r2 <- iftp_quantile(
      time_steps = 10, remove_per_step = 10,
      cdf = dist_exponential(), time_horizon = 2,
      method = "order_statistics", seed = 42
    )
  })
  expect_equal(r1, r2)
})

test_that("iftp_quantile order_statistics uses n_total = time_steps * remove_per_step", {
  result <- iftp_quantile(
    time_steps = 20, remove_per_step = 5,
    cdf = dist_exponential(), time_horizon = 2,
    method = "order_statistics", seed = 42
  )
  expect_true(nrow(result$boundary) <= 21)
  expect_true(nrow(result$boundary) >= 2)
})

test_that("iftp_quantile order_statistics time grid is sorted and starts at 0", {
  result <- iftp_quantile(
    time_steps = 20, remove_per_step = 10,
    cdf = dist_exponential(), time_horizon = 3,
    method = "order_statistics", seed = 1
  )
  expect_equal(result$boundary$time[1], 0)
  expect_true(all(diff(result$boundary$time) > 0))
  expect_true(all(result$boundary$time <= 3))
})

test_that("iftp_quantile order_statistics errors for dist without sampler", {
  expect_error(
    iftp_quantile(
      time_steps = 10, remove_per_step = 10,
      cdf = dist_soft_killing_constant_barrier(),
      time_horizon = 2, method = "order_statistics", seed = 1
    ),
    "sampling function"
  )
})

test_that("iftp_quantile order_statistics errors for raw CDF", {
  expect_error(
    iftp_quantile(
      time_steps = 10, remove_per_step = 10,
      cdf = function(x) pexp(x),
      qf = function(p) qexp(p),
      time_horizon = 2, method = "order_statistics", seed = 1
    ),
    "sampling function"
  )
})

test_that("iftp_quantile order_statistics differs from quantiles", {
  r_q <- iftp_quantile(
    time_steps = 20, remove_per_step = 10,
    cdf = dist_exponential(), time_horizon = 3,
    method = "quantiles", seed = 42
  )
  expect_warning(
    r_os <- iftp_quantile(
      time_steps = 20, remove_per_step = 10,
      cdf = dist_exponential(), time_horizon = 3,
      method = "order_statistics", seed = 42
    ),
    "Insufficient particles"
  )
  expect_false(identical(r_q$boundary$time, r_os$boundary$time))
})
