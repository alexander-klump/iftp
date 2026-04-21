test_that("iftp_soft_killing returns iftp_result class", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 100, seed = 1
  )
  expect_s3_class(result, "iftp_result")
  expect_s3_class(result$boundary, "data.frame")
  expect_named(result$boundary, c("time", "boundary"))
  expect_equal(result$params$algorithm, "soft_killing")
  expect_true("cost" %in% names(result$params))
})

test_that("iftp_soft_killing returns correct structure", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 100, seed = 1
  )
  expect_true(is.na(result$boundary$boundary[1]))
})

test_that("iftp_soft_killing is reproducible with seed", {
  r1 <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 500, seed = 42
  )
  r2 <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 500, seed = 42
  )
  expect_equal(r1, r2)
})

test_that("iftp_soft_killing equidistant grid has correct spacing", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 16,
    n_particles = 100, seed = 1
  )
  diffs <- diff(result$boundary$time)
  expected_spacing <- 1 / 16
  expect_true(all(abs(diffs - expected_spacing) < 1e-12))
})

test_that("iftp_soft_killing custom init works", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8, n_particles = 100,
    init = function(n) rnorm(n, sd = 0.1), seed = 1
  )
  expect_s3_class(result, "iftp_result")
})

test_that("iftp_soft_killing accepts custom process", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8, n_particles = 100,
    process = process_bm(sigma = 2), seed = 1
  )
  expect_s3_class(result, "iftp_result")
})

test_that("iftp_soft_killing accepts cost = abs", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8, n_particles = 100,
    cost = abs, seed = 1
  )
  expect_s3_class(result, "iftp_result")
})

test_that("iftp_soft_killing validates inputs", {
  expect_error(iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = -1, time_steps = 8,
    n_particles = 100
  ))
  expect_error(iftp_soft_killing(
    cdf = "bad", time_horizon = 1, time_steps = 8,
    n_particles = 100
  ))
  expect_error(iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 100, init = "bad"
  ))
  expect_error(iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 100, process = "bad"
  ))
  expect_error(iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 100, cost = "bad"
  ))
})

test_that("weighted_cost_quantile is correct for equal weights", {
  x <- 1:10
  w <- rep(1 / 10, 10)
  q <- weighted_cost_quantile(x, 0.5, w, identity)
  expect_true(q >= 5 && q <= 6)
})

test_that("weighted_cost_quantile handles edge cases", {
  x <- c(1, 2, 3)
  w <- c(0.5, 0.3, 0.2)
  expect_equal(weighted_cost_quantile(x, -0.1, w, identity), 1)
  expect_equal(weighted_cost_quantile(x, 1.0, w, identity), 3)
})

test_that("weighted_cost_quantile sorts by cost, returns position", {
  x <- c(3, -1, 2, -4)
  w <- rep(0.25, 4)
  # cost = abs: sorted by |x| → {-1, 2, 3, -4}, cumsum = {0.25, 0.5, 0.75, 1}
  # prob = 0.5: first cumsum >= 0.5 is idx 2 → position 2
  q <- weighted_cost_quantile(x, 0.5, w, abs)
  expect_equal(q, 2)
  # cost = identity: sorted by x → {-4, -1, 2, 3}
  # prob = 0.5: first cumsum >= 0.5 is idx 2 → position -1
  q2 <- weighted_cost_quantile(x, 0.5, w, identity)
  expect_equal(q2, -1)
})

test_that("print.iftp_result works for soft_killing", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 100, seed = 1
  )
  expect_output(print(result), "soft_killing")
})

test_that("iftp_soft_killing default cost and cost = abs differ", {
  r_default <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 200, seed = 42
  )
  r_abs <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 200, cost = abs, seed = 42
  )
  expect_false(identical(r_default$boundary$boundary, r_abs$boundary$boundary))
})

test_that("iftp_soft_killing custom cost function works", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 100, cost = function(x) x^2, seed = 1
  )
  expect_s3_class(result, "iftp_result")
})

test_that("iftp_soft_killing handles CDF reaching 1", {
  result <- iftp_soft_killing(
    cdf = dist_uniform(0, 1), time_horizon = 2, time_steps = 16,
    n_particles = 100, seed = 1
  )
  expect_s3_class(result, "iftp_result")
  late <- result$boundary$time > 1
  expect_true(any(late))
  expect_true(all(is.na(result$boundary$boundary[late])))
})

test_that("iftp_soft_killing default cost is function(x) -x", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 100, seed = 1
  )
  expect_equal(result$params$cost(c(-2, 0, 3)), c(2, 0, -3))
})

test_that("iftp_soft_killing aborts on NaN from process", {
  bad_process <- function(x, dt) rep(NaN, length(x))
  class(bad_process) <- "iftp_process"
  attr(bad_process, "process_type") <- "bad"
  expect_error(
    iftp_soft_killing(
      cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
      n_particles = 100, process = bad_process, seed = 1
    ),
    "non-finite"
  )
})

test_that("iftp_soft_killing works with BM+Poisson process", {
  result <- iftp_soft_killing(
    cdf = dist_exponential(), time_horizon = 1, time_steps = 8,
    n_particles = 100,
    process = process_bm_drift_poisson(), cost = identity, seed = 1
  )
  expect_s3_class(result, "iftp_result")
})
