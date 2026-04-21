test_that("iftp_anulova returns iftp_result class", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  expect_s3_class(result, "iftp_result")
  expect_s3_class(result$boundary, "data.frame")
  expect_named(result$boundary, c("time", "boundary"))
  expect_true(is.list(result$params))
  expect_true("cost" %in% names(result$params))
  expect_true("process" %in% names(result$params))
  expect_true("init" %in% names(result$params))
  expect_equal(result$params$algorithm, "anulova")
})

test_that("iftp_anulova returns correct dimensions", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  expect_equal(nrow(result$boundary), 11)
  expect_equal(result$boundary$time[1], 0)
  expect_equal(result$boundary$boundary[1], 0)
})

test_that("iftp_anulova is reproducible with seed", {
  r1 <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 20,
    n_particles = 200, seed = 42
  )
  r2 <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 20,
    n_particles = 200, seed = 42
  )
  expect_equal(r1, r2)
})

test_that("iftp_anulova produces non-trivial boundary", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 2, time_steps = 50,
    n_particles = 500, cost = abs, seed = 1
  )
  expect_true(any(result$boundary$boundary[-1] != 0))
})

test_that("iftp_anulova works with cost = identity", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 20,
    n_particles = 200, cost = identity, seed = 1
  )
  expect_equal(nrow(result$boundary), 21)
})

test_that("iftp_anulova cost = abs and cost = identity differ", {
  r_abs <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 20,
    n_particles = 200, cost = abs, seed = 42
  )
  r_id <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 20,
    n_particles = 200, cost = identity, seed = 42
  )
  expect_false(identical(r_abs$boundary$boundary, r_id$boundary$boundary))
})

test_that("iftp_anulova accepts raw CDF function", {
  result <- iftp_anulova(function(x) pexp(x),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  expect_s3_class(result, "iftp_result")
})

test_that("iftp_anulova works with BM+Poisson process", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100,
    process = process_bm_drift_poisson(), cost = identity,
    seed = 1
  )
  expect_equal(nrow(result$boundary), 11)
})

test_that("iftp_anulova works with Gamma process", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100,
    process = process_negative_gamma(), cost = identity,
    seed = 1
  )
  expect_equal(nrow(result$boundary), 11)
})

test_that("iftp_anulova validates inputs", {
  expect_error(iftp_anulova("not_a_function",
    time_horizon = 1, time_steps = 10,
    n_particles = 100
  ))
  expect_error(iftp_anulova(dist_exponential(),
    time_horizon = -1,
    time_steps = 10, n_particles = 100
  ))
  expect_error(iftp_anulova(dist_exponential(),
    time_horizon = 1,
    time_steps = 0, n_particles = 100
  ))
  expect_error(iftp_anulova(dist_exponential(),
    time_horizon = 1,
    time_steps = 10, n_particles = 5
  ))
  expect_error(iftp_anulova(dist_exponential(),
    time_horizon = 1,
    time_steps = 10, n_particles = 100, cost = "abs"
  ))
})

test_that("iftp_anulova two-sided converges toward Shiryaev constant", {
  shiryaev <- pi / (2 * sqrt(2))
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 5, time_steps = 200,
    n_particles = 5000, cost = abs, seed = 123
  )
  late_boundary <- abs(tail(result$boundary$boundary, 50))
  expect_true(mean(late_boundary) > shiryaev * 0.7)
  expect_true(mean(late_boundary) < shiryaev * 1.5)
})

test_that("iftp_anulova stops early when CDF reaches 1", {
  result <- iftp_anulova(dist_uniform(0, 1),
    time_horizon = 2, time_steps = 20,
    n_particles = 200, seed = 1
  )
  late_times <- result$boundary$time > 1
  if (any(late_times)) {
    expect_true(all(result$boundary$boundary[late_times] == 0))
  }
})

test_that("iftp_anulova custom cost function works", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100,
    cost = function(x) x^2, seed = 1
  )
  expect_s3_class(result, "iftp_result")
})

test_that("iftp_anulova warns on particle depletion", {
  env <- new.env(parent = emptyenv())
  env$n <- 0L
  depleting_process <- function(x, delta_t) {
    env$n <- env$n + 1L
    if (env$n >= 2L) {
      return(numeric(0))
    }
    x + rnorm(length(x), 0, sqrt(delta_t))
  }
  class(depleting_process) <- "iftp_process"
  attr(depleting_process, "process_type") <- "depleting"
  expect_warning(
    iftp_anulova(
      cdf = dist_exponential(), time_horizon = 2, time_steps = 10,
      n_particles = 10, process = depleting_process, seed = 42
    ),
    "depleted"
  )
})

test_that("iftp_anulova custom init shifts boundary", {
  r_default <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 20,
    n_particles = 200, seed = 42
  )
  r_shifted <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 20,
    n_particles = 200,
    init = function(n) rep(1, n), seed = 42
  )
  expect_s3_class(r_shifted, "iftp_result")
  expect_false(identical(r_default$boundary$boundary, r_shifted$boundary$boundary))
})

test_that("iftp_anulova default cost is abs", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  expect_identical(result$params$cost, abs)
})

test_that("iftp_anulova aborts on NaN from process", {
  bad_process <- function(x, dt) rep(NaN, length(x))
  class(bad_process) <- "iftp_process"
  attr(bad_process, "process_type") <- "bad"
  expect_error(
    iftp_anulova(
      dist_exponential(),
      time_horizon = 1, time_steps = 10,
      n_particles = 100, process = bad_process, seed = 1
    ),
    "non-finite"
  )
})

test_that("print.iftp_result works for anulova", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  expect_output(print(result), "anulova")
})
