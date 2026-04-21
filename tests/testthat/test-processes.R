test_that("process_bm returns correct length vector", {
  bm <- process_bm()
  x <- rep(0, 100)
  x_new <- bm(x, 0.01)
  expect_length(x_new, 100)
  expect_type(x_new, "double")
})

test_that("process_bm respects sigma", {
  bm1 <- process_bm(sigma = 1)
  bm10 <- process_bm(sigma = 10)
  x <- rep(0, 10000)
  set.seed(1)
  x1 <- bm1(x, 1)
  set.seed(1)
  x10 <- bm10(x, 1)
  expect_equal(x10, x1 * 10)
})

test_that("process_bm_drift_poisson returns correct length", {
  proc <- process_bm_drift_poisson()
  x_new <- proc(rep(0, 100), 0.01)
  expect_length(x_new, 100)
})

test_that("process_bm_drift_poisson includes drift", {
  mu <- 3.7
  dt <- 0.5
  proc_drift <- process_bm_drift_poisson(mu = mu)
  proc_nodrift <- process_bm_drift_poisson(mu = 0)
  x <- rep(0, 100)
  set.seed(42)
  x_drift <- proc_drift(x, dt)
  set.seed(42)
  x_nodrift <- proc_nodrift(x, dt)
  # Strip jump_size attribute before comparing: we only test the numeric values
  expect_equal(as.numeric(x_drift - x_nodrift), rep(mu * dt, 100))
})

test_that("process_gamma returns positions shifted negatively", {
  gp <- process_negative_gamma()
  x <- rep(0, 1000)
  set.seed(42)
  x_new <- gp(x, 0.1)
  expect_true(all(x_new <= 0))
  expect_length(x_new, 1000)
})

test_that("process adds to current positions", {
  bm <- process_bm()
  x <- rep(5, 100)
  set.seed(1)
  x_new <- bm(x, 0.01)
  # all positions should be near 5, not near 0
  expect_true(abs(mean(x_new) - 5) < 0.5)
})

test_that("process factories have iftp_process class", {
  expect_s3_class(process_bm(), "iftp_process")
  expect_s3_class(process_bm_drift_poisson(), "iftp_process")
  expect_s3_class(process_negative_gamma(), "iftp_process")
})

test_that("process factories reject invalid parameters", {
  expect_error(process_bm(sigma = -1))
  expect_error(process_bm(sigma = "a"))
  expect_error(process_bm_drift_poisson(poisson_intensity = -1))
  expect_error(process_negative_gamma(rate = -1))
})

test_that("validate_process rejects non-functions", {
  expect_error(validate_process(42))
  expect_error(validate_process(function(x) x))
})

test_that("validate_process accepts valid functions", {
  expect_invisible(validate_process(function(x, dt) x + rnorm(length(x))))
  expect_invisible(validate_process(process_bm()))
})

# --- validate_cost() ---

test_that("validate_cost accepts identity", {
  expect_silent(validate_cost(identity))
})

test_that("validate_cost accepts abs", {
  expect_silent(validate_cost(abs))
})

test_that("validate_cost accepts custom function", {
  expect_silent(validate_cost(function(x) -x))
})

test_that("validate_cost rejects non-function", {
  expect_error(validate_cost("abs"), "must be a function")
})

test_that("validate_cost rejects wrong output length", {
  expect_error(validate_cost(function(x) sum(x)), "same length")
})

test_that("validate_cost rejects non-numeric output", {
  expect_error(validate_cost(function(x) as.character(x)), "numeric vector")
})

test_that("process_bm_drift_poisson has positive mean increment from jumps", {
  proc <- process_bm_drift_poisson(sigma = 0.01, poisson_intensity = 10)
  set.seed(42)
  x <- rep(0, 10000)
  increments <- proc(x, 1) - x
  # Expected mean: poisson_intensity * dt = 10 (from Poisson component)
  expect_gt(mean(increments), 5)
})

test_that("process_bm_drift_poisson mean increment reflects drift", {
  proc <- process_bm_drift_poisson(mu = 5, sigma = 0.01, poisson_intensity = 0.01)
  set.seed(42)
  x <- rep(0, 10000)
  increments <- proc(x, 1) - x
  expect_gt(mean(increments), 4)
  expect_lt(mean(increments), 6)
})

test_that("process_negative_gamma rate affects increment magnitude", {
  slow <- process_negative_gamma(rate = 0.1)
  fast <- process_negative_gamma(rate = 10)
  set.seed(42)
  x <- rep(0, 10000)
  inc_slow <- slow(x, 1)
  set.seed(42)
  inc_fast <- fast(x, 1)
  expect_gt(abs(mean(inc_fast)), abs(mean(inc_slow)))
})

test_that("process_bm returns no jump_size attribute", {
  proc <- process_bm()
  x_new <- proc(rep(0, 10), 0.01)
  expect_null(attr(x_new, "jump_size"))
})

test_that("process_bm_drift_poisson returns jump_size attribute", {
  proc <- process_bm_drift_poisson(poisson_intensity = 10)
  set.seed(42)
  x <- rep(0, 100)
  x_new <- proc(x, 1)
  js <- attr(x_new, "jump_size")
  expect_false(is.null(js))
  expect_length(js, 100)
  expect_type(js, "double")
  # jump_size should be non-negative integers (Poisson)
  expect_true(all(js >= 0))
  expect_true(all(js == floor(js)))
  # With high intensity, most particles should have jumped
  expect_true(sum(js > 0) > 50)
})

test_that("process_bm_drift_poisson returns jump_size attribute", {
  proc <- process_bm_drift_poisson(mu = 1, poisson_intensity = 10)
  set.seed(42)
  x <- rep(0, 100)
  x_new <- proc(x, 1)
  js <- attr(x_new, "jump_size")
  expect_false(is.null(js))
  expect_length(js, 100)
  expect_type(js, "double")
})

test_that("process_negative_gamma returns no jump_size attribute", {
  proc <- process_negative_gamma()
  x_new <- proc(rep(0, 10), 0.01)
  expect_null(attr(x_new, "jump_size"))
})

test_that("process_bm_drift_poisson jump_size matches Poisson component", {
  proc <- process_bm_drift_poisson(sigma = 1, poisson_intensity = 5)
  set.seed(42)
  x <- rep(0, 1000)
  x_new <- proc(x, 1)
  js <- attr(x_new, "jump_size")
  # Pre-jump position should differ from x only by BM component
  pre_jump <- x_new - js
  # pre_jump - x is the BM increment: mean ≈ 0, sd ≈ sigma * sqrt(dt) = 1
  bm_part <- pre_jump - x
  expect_true(abs(mean(bm_part)) < 0.2)
})

# --- Diffusion processes: process_ou() ---

test_that("process_ou returns correct length vector", {
  ou <- process_ou()
  x <- rep(0, 100)
  x_new <- ou(x, 0.01)
  expect_length(x_new, 100)
  expect_type(x_new, "double")
})

test_that("process_ou has iftp_process class", {
  ou <- process_ou()
  expect_s3_class(ou, "iftp_process")
  expect_equal(attr(ou, "process_type"), "ou")
})

test_that("process_ou rejects invalid parameters", {
  expect_error(process_ou(theta = -1))
  expect_error(process_ou(theta = "a"))
  expect_error(process_ou(sigma = 0))
  expect_error(process_ou(mu = "x"))
})

test_that("process_ou mean-reverts toward mu", {
  mu_target <- 3
  ou <- process_ou(theta = 5, mu = mu_target, sigma = 0.5)
  x <- rep(0, 10000)
  set.seed(42)
  for (i in seq_len(200)) x <- ou(x, 0.01)
  expect_true(abs(mean(x) - mu_target) < 0.5)
})

# --- Diffusion processes: process_cev() ---

test_that("process_cev returns correct length vector", {
  cev <- process_cev()
  x <- rep(1, 100)
  x_new <- cev(x, 0.01)
  expect_length(x_new, 100)
  expect_type(x_new, "double")
})

test_that("process_cev has iftp_process class", {
  cev <- process_cev()
  expect_s3_class(cev, "iftp_process")
  expect_equal(attr(cev, "process_type"), "cev")
})

test_that("process_cev rejects invalid parameters", {
  expect_error(process_cev(sigma = -1))
  expect_error(process_cev(sigma = 0))
  expect_error(process_cev(mu = "x"))
  expect_error(process_cev(gamma = 0))
})

test_that("process_cev stays positive", {
  cev <- process_cev(mu = 0, sigma = 1, gamma = 1)
  x <- rep(1, 1000)
  set.seed(42)
  for (i in seq_len(100)) x <- cev(x, 0.01)
  expect_true(all(x > 0))
})

# --- Diffusion processes: process_cir() ---

test_that("process_cir returns correct length vector", {
  cir <- process_cir()
  x <- rep(1, 100)
  x_new <- cir(x, 0.01)
  expect_length(x_new, 100)
  expect_type(x_new, "double")
})

test_that("process_cir has iftp_process class", {
  cir <- process_cir()
  expect_s3_class(cir, "iftp_process")
  expect_equal(attr(cir, "process_type"), "cir")
})

test_that("process_cir rejects invalid parameters", {
  expect_error(process_cir(kappa = -1))
  expect_error(process_cir(theta = 0))
  expect_error(process_cir(sigma = -1))
})

test_that("process_cir stays non-negative", {
  cir <- process_cir(kappa = 1, theta = 1, sigma = 1)
  x <- rep(1, 1000)
  set.seed(42)
  for (i in seq_len(100)) x <- cir(x, 0.01)
  expect_true(all(x >= 0))
})

# --- Diffusion processes: process_bessel() ---

test_that("process_bessel returns correct length vector", {
  bes <- process_bessel()
  x <- rep(1, 100)
  x_new <- bes(x, 0.01)
  expect_length(x_new, 100)
  expect_type(x_new, "double")
})

test_that("process_bessel has iftp_process class", {
  bes <- process_bessel()
  expect_s3_class(bes, "iftp_process")
  expect_equal(attr(bes, "process_type"), "bessel")
})

test_that("process_bessel rejects invalid parameters", {
  expect_error(process_bessel(delta = -1))
  expect_error(process_bessel(delta = 0))
  expect_error(process_bessel(delta = "x"))
})

test_that("process_bessel stays positive", {
  bes <- process_bessel(delta = 3)
  x <- rep(1, 1000)
  set.seed(42)
  for (i in seq_len(100)) x <- bes(x, 0.01)
  expect_true(all(x > 0))
})

# --- Diffusion factories: smoke test with iftp_anulova ---

# --- Levy processes: process_cauchy() ---

test_that("process_cauchy returns correct length vector", {
  cauchy <- process_cauchy()
  x <- rep(0, 100)
  x_new <- cauchy(x, 0.01)
  expect_length(x_new, 100)
  expect_type(x_new, "double")
})

test_that("process_cauchy has iftp_process class", {
  cauchy <- process_cauchy()
  expect_s3_class(cauchy, "iftp_process")
  expect_equal(attr(cauchy, "process_type"), "cauchy")
})

test_that("process_cauchy rejects invalid parameters", {
  expect_error(process_cauchy(gamma = -1))
  expect_error(process_cauchy(gamma = 0))
})

# --- Diffusion processes: process_wright_fisher() ---

test_that("process_wright_fisher returns correct length vector", {
  wf <- process_wright_fisher()
  x <- rep(0.5, 100)
  x_new <- wf(x, 0.01)
  expect_length(x_new, 100)
  expect_type(x_new, "double")
})

test_that("process_wright_fisher has iftp_process class", {
  wf <- process_wright_fisher()
  expect_s3_class(wf, "iftp_process")
  expect_equal(attr(wf, "process_type"), "wright_fisher")
})

test_that("process_wright_fisher rejects invalid parameters", {
  expect_error(process_wright_fisher(theta1 = -1))
  expect_error(process_wright_fisher(theta2 = 0))
  expect_error(process_wright_fisher(lower = 1, upper = 0))
  expect_error(process_wright_fisher(lower = 1, upper = 1))
})

test_that("process_wright_fisher stays in [0, 1]", {
  wf <- process_wright_fisher(theta1 = 1, theta2 = 1)
  x <- rep(0.5, 1000)
  set.seed(42)
  for (i in seq_len(100)) x <- wf(x, 0.01)
  expect_true(all(x >= 0 & x <= 1))
})

test_that("process_wright_fisher respects custom interval", {
  wf <- process_wright_fisher(theta1 = 1, theta2 = 1, lower = 2, upper = 5)
  x <- rep(3.5, 1000)
  set.seed(42)
  for (i in seq_len(100)) x <- wf(x, 0.01)
  expect_true(all(x >= 2 & x <= 5))
})

# --- Smoke test with iftp_anulova ---

test_that("new processes work with iftp_anulova", {
  dist <- dist_exponential(rate = 1)
  for (proc in list(
    process_cauchy(gamma = 0.1),
    process_ou(theta = 1, mu = 0, sigma = 1),
    process_cev(mu = 0, sigma = 0.5, gamma = 1),
    process_cir(kappa = 1, theta = 1, sigma = 0.5),
    process_bessel(delta = 3),
    process_wright_fisher(theta1 = 1, theta2 = 1)
  )) {
    result <- iftp_anulova(
      cdf = dist, time_horizon = 1, time_steps = 10,
      n_particles = 50, process = proc, seed = 42
    )
    expect_s3_class(result, "iftp_result")
  }
})
