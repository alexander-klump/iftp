# --- Exponential -----------------------------------------------------------

test_that("dist_exponential roundtrips", {
  d <- dist_exponential(rate = 2)
  for (x in c(0.1, 1.0, 3.0)) {
    expect_equal(d$qf(d$cdf(x)), x, tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(d$qf(p)), p, tolerance = 1e-10)
  }
})

test_that("dist_exponential matches R builtins", {
  d <- dist_exponential(rate = 2)
  for (x in c(0.1, 1.0, 3.0)) {
    expect_equal(d$cdf(x), pexp(x, rate = 2), tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$qf(p), qexp(p, rate = 2), tolerance = 1e-10)
  }
})

# --- Weibull ---------------------------------------------------------------

test_that("dist_weibull roundtrips", {
  d <- dist_weibull(shape = 2, scale = 3)
  for (x in c(0.5, 2.0, 5.0)) {
    expect_equal(d$qf(d$cdf(x)), x, tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(d$qf(p)), p, tolerance = 1e-10)
  }
})

test_that("dist_weibull matches R builtins", {
  d <- dist_weibull(shape = 2, scale = 3)
  for (x in c(0.5, 2.0, 5.0)) {
    expect_equal(d$cdf(x), pweibull(x, shape = 2, scale = 3),
      tolerance = 1e-10
    )
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$qf(p), qweibull(p, shape = 2, scale = 3),
      tolerance = 1e-10
    )
  }
})

# --- Log-logistic ----------------------------------------------------------

test_that("dist_log_logistic roundtrips", {
  d <- dist_log_logistic(shape = 8, scale = 2)
  for (x in c(0.5, 2.0, 5.0)) {
    expect_equal(d$qf(d$cdf(x)), x, tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(d$qf(p)), p, tolerance = 1e-10)
  }
})

test_that("dist_log_logistic matches flexsurv", {
  d <- dist_log_logistic(shape = 8, scale = 2)
  for (x in c(0.5, 2.0, 5.0)) {
    expect_equal(d$cdf(x), flexsurv::pllogis(x, shape = 8, scale = 2),
      tolerance = 1e-10
    )
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$qf(p), flexsurv::qllogis(p, shape = 8, scale = 2),
      tolerance = 1e-10
    )
  }
})

# --- Frechet ---------------------------------------------------------------

test_that("dist_frechet roundtrips", {
  d <- dist_frechet(shape = 2, scale = 3)
  for (x in c(0.5, 2.0, 10.0)) {
    expect_equal(d$qf(d$cdf(x)), x, tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(d$qf(p)), p, tolerance = 1e-10)
  }
})

test_that("dist_frechet CDF matches formula", {
  d <- dist_frechet(shape = 2, scale = 3)
  for (x in c(0.5, 2.0, 10.0)) {
    expect_equal(d$cdf(x), exp(-(x / 3)^(-2)), tolerance = 1e-10)
  }
  expect_equal(d$cdf(0), 0)
  expect_equal(d$cdf(-1), 0)
})

# --- Lomax -----------------------------------------------------------------

test_that("dist_lomax roundtrips", {
  d <- dist_lomax(shape = 2, scale = 3)
  for (x in c(0.5, 2.0, 10.0)) {
    expect_equal(d$qf(d$cdf(x)), x, tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(d$qf(p)), p, tolerance = 1e-10)
  }
})

test_that("dist_lomax CDF matches formula", {
  d <- dist_lomax(shape = 2, scale = 3)
  for (x in c(0.5, 2.0, 10.0)) {
    expect_equal(d$cdf(x), 1 - (1 + x / 3)^(-2), tolerance = 1e-10)
  }
  expect_equal(d$cdf(0), 0)
  expect_equal(d$cdf(-1), 0)
})

# --- Gamma -----------------------------------------------------------------

test_that("dist_gamma roundtrips", {
  d <- dist_gamma(shape = 2, rate = 3)
  for (x in c(0.1, 1.0, 3.0)) {
    expect_equal(d$qf(d$cdf(x)), x, tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(d$qf(p)), p, tolerance = 1e-10)
  }
})

test_that("dist_gamma matches R builtins", {
  d <- dist_gamma(shape = 2, rate = 3)
  for (x in c(0.1, 1.0, 3.0)) {
    expect_equal(d$cdf(x), pgamma(x, shape = 2, rate = 3),
      tolerance = 1e-10
    )
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$qf(p), qgamma(p, shape = 2, rate = 3),
      tolerance = 1e-10
    )
  }
})

# --- Uniform ---------------------------------------------------------------

test_that("dist_uniform roundtrips", {
  d <- dist_uniform(min = 1, max = 5)
  for (x in c(1.5, 3.0, 4.5)) {
    expect_equal(d$qf(d$cdf(x)), x, tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(d$qf(p)), p, tolerance = 1e-10)
  }
})

test_that("dist_uniform matches R builtins", {
  d <- dist_uniform(min = 1, max = 5)
  for (x in c(1.5, 3.0, 4.5)) {
    expect_equal(d$cdf(x), punif(x, min = 1, max = 5), tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$qf(p), qunif(p, min = 1, max = 5),
      tolerance = 1e-10
    )
  }
})

# --- Arcsine ---------------------------------------------------------------

test_that("dist_arcsine roundtrips", {
  d <- dist_arcsine()
  for (x in c(0.1, 0.5, 0.9)) {
    expect_equal(d$qf(d$cdf(x)), x, tolerance = 1e-10)
  }
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(d$qf(p)), p, tolerance = 1e-10)
  }
})

test_that("dist_arcsine CDF matches formula", {
  d <- dist_arcsine()
  for (x in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(x), (2 / pi) * asin(sqrt(x)), tolerance = 1e-10)
  }
  expect_equal(d$cdf(0), 0)
  expect_equal(d$cdf(1), 1, tolerance = 1e-10)
})

# --- Soft-killing (constant barrier) ----------------------------------------

test_that("dist_soft_killing_constant_barrier rejects invalid barrier", {
  expect_error(
    dist_soft_killing_constant_barrier(barrier = 0),
    "nonzero"
  )
  expect_error(dist_soft_killing_constant_barrier(barrier = "a"))
  expect_error(dist_soft_killing_constant_barrier(barrier = Inf))
  expect_error(dist_soft_killing_constant_barrier(barrier = NA))
})

test_that("dist_soft_killing_constant_barrier rejects invalid killing_rate", {
  expect_error(dist_soft_killing_constant_barrier(killing_rate = 0))
  expect_error(dist_soft_killing_constant_barrier(killing_rate = -1))
  expect_error(dist_soft_killing_constant_barrier(killing_rate = NA))
})

test_that("dist_soft_killing_constant_barrier returns iftp_dist", {
  d <- dist_soft_killing_constant_barrier()
  expect_s3_class(d, "iftp_dist")
})

test_that("dist_soft_killing_constant_barrier CDF boundary values", {
  d <- dist_soft_killing_constant_barrier(barrier = 1)
  # F(t) = 0 for t <= 0
  expect_equal(d$cdf(0), 0)
  expect_equal(d$cdf(-1), 0)
  # F(t) approaches 1 for large t
  expect_gt(d$cdf(50), 0.97)
  # F(t) is in [0, 1] and monotone increasing
  vals <- d$cdf(c(0.5, 1, 2, 5, 10))
  expect_true(all(vals >= 0 & vals <= 1))
  expect_true(all(diff(vals) > 0))
})

test_that("dist_soft_killing_constant_barrier CDF is vectorized", {
  d <- dist_soft_killing_constant_barrier(barrier = 1)
  t_vals <- c(-1, 0, 0.5, 1, 5)
  result <- d$cdf(t_vals)
  expect_length(result, 5L)
  expect_equal(result[1:2], c(0, 0))
  expect_true(all(result[3:5] > 0))
})

test_that("dist_soft_killing_constant_barrier negative barrier works", {
  d <- dist_soft_killing_constant_barrier(barrier = -1)
  # alpha = 0 for negative barrier, so no exponential decay
  vals <- d$cdf(c(0.5, 1, 2, 5, 10))
  expect_true(all(vals >= 0 & vals <= 1))
  expect_true(all(diff(vals) > 0))
})

test_that("dist_soft_killing_constant_barrier killing_rate shifts distribution", {
  d_slow <- dist_soft_killing_constant_barrier(barrier = 1, killing_rate = 0.5)
  d_fast <- dist_soft_killing_constant_barrier(barrier = 1, killing_rate = 2)
  # Higher killing rate => faster killing => CDF reaches values sooner
  expect_gt(d_fast$cdf(1), d_slow$cdf(1))
})

test_that("dist_soft_killing_constant_barrier quantile roundtrips", {
  d <- dist_soft_killing_constant_barrier(barrier = 1)
  for (p in c(0.1, 0.5, 0.9)) {
    expect_equal(d$cdf(d$qf(p)), p, tolerance = 1e-4)
  }
})

test_that("dist_soft_killing_constant_barrier prints nicely", {
  d <- dist_soft_killing_constant_barrier(barrier = 2, killing_rate = 0.5)
  expect_output(print(d), "Soft-killing")
})

# --- Validation and utilities ----------------------------------------------

test_that("distribution factories reject invalid params", {
  expect_error(dist_exponential(rate = -1))
  expect_error(dist_weibull(shape = 0))
  expect_error(dist_log_logistic(shape = -1))
  expect_error(dist_lomax(shape = -1))
  expect_error(dist_gamma(rate = 0))
  expect_error(dist_uniform(min = 5, max = 2))
  expect_error(dist_frechet(shape = 0))
  expect_error(dist_frechet(shape = -1))
  expect_error(dist_frechet(scale = 0))
  expect_error(dist_frechet(scale = -1))
  expect_error(dist_soft_killing_constant_barrier(barrier = 0))
  expect_error(dist_soft_killing_constant_barrier(killing_rate = -1))
})

test_that("iftp_dist objects print nicely", {
  d <- dist_exponential()
  expect_output(print(d), "Exponential")
})

test_that("extract_cdf handles both types", {
  d <- dist_exponential()
  expect_true(is.function(extract_cdf(d)))
  f <- function(x) pexp(x)
  expect_true(is.function(extract_cdf(f)))
  expect_error(extract_cdf(42))
})

test_that("extract_sample returns NULL for raw function", {
  rf <- extract_sample(function(x) pexp(x))
  expect_null(rf)
})

test_that("extract_sample returns NULL for iftp_dist without sampler", {
  d <- dist_soft_killing_constant_barrier()
  expect_null(extract_sample(d))
})

test_that("iftp_dist print shows sample not available when NULL", {
  d_no <- dist_soft_killing_constant_barrier()
  expect_output(print(d_no), "Sample function: not available")
})

# --- Sample functions -------------------------------------------------------

test_that("iftp_dist objects include rf field", {
  d <- dist_exponential()
  expect_true(is.function(d$rf))
  expect_length(d$rf(10), 10)
  expect_true(all(d$rf(100) > 0))
})

test_that("iftp_dist print shows sample availability", {
  d <- dist_exponential()
  expect_output(print(d), "Sample function: available")
})

test_that("extract_sample returns function for iftp_dist with sampler", {
  d <- dist_exponential()
  rf <- extract_sample(d)
  expect_true(is.function(rf))
})

test_that("all dist factories with samplers produce valid samples", {
  dists <- list(
    dist_exponential(rate = 2),
    dist_weibull(shape = 2, scale = 1),
    dist_gamma(shape = 2, rate = 1),
    dist_uniform(min = 0, max = 2),
    dist_lomax(shape = 2, scale = 1),
    dist_frechet(shape = 2, scale = 1),
    dist_arcsine()
  )
  for (d in dists) {
    expect_true(is.function(d$rf), info = d$name)
    s <- d$rf(1000)
    expect_length(s, 1000)
    expect_true(all(is.finite(s)), info = d$name)
  }
})

test_that("log-logistic sampler works", {
  skip_if_not_installed("flexsurv")
  d <- dist_log_logistic(shape = 2, scale = 1)
  expect_true(is.function(d$rf))
  s <- d$rf(100)
  expect_length(s, 100)
  expect_true(all(is.finite(s)))
})

test_that("soft_killing_constant_barrier has NULL rf", {
  d <- dist_soft_killing_constant_barrier()
  expect_null(d$rf)
})

test_that("sampled values are consistent with CDF", {
  set.seed(42)
  d <- dist_exponential(rate = 2)
  s <- d$rf(5000)
  ks <- ks.test(s, d$cdf)
  expect_true(ks$p.value > 0.01)
})

# --- extract_quantile -------------------------------------------------------

test_that("extract_quantile extracts from iftp_dist", {
  d <- dist_exponential()
  qf <- extract_quantile(d)
  expect_true(is.function(qf))
  expect_equal(qf(0.5), qexp(0.5))
})

test_that("extract_quantile uses explicit qf over iftp_dist", {
  d <- dist_exponential()
  custom_qf <- function(p) p * 10
  qf <- extract_quantile(d, qf = custom_qf)
  expect_equal(qf(0.5), 5)
})

test_that("extract_quantile errors without qf", {
  expect_error(extract_quantile(function(x) pexp(x)), "quantile function")
})

# --- build_time_grid ----------------------------------------------------------

test_that("build_time_grid quantiles method matches manual computation", {
  d <- dist_exponential(rate = 1)
  grid <- build_time_grid(
    cdf = d$cdf, qf = d$qf, rf = NULL,
    n_steps = 10, time_horizon = 3, method = "quantiles"
  )
  expected_max <- floor(pexp(3) * 10)
  expected_probs <- seq(1 / 10, expected_max / 10, by = 1 / 10)
  expected <- c(0, qexp(expected_probs))
  expect_equal(grid, expected)
})

test_that("build_time_grid quantiles replaces trailing Inf", {
  d <- dist_exponential(rate = 1)
  grid <- build_time_grid(
    cdf = d$cdf, qf = d$qf, rf = NULL,
    n_steps = 10, time_horizon = 100, method = "quantiles"
  )
  expect_true(all(is.finite(grid)))
})

test_that("build_time_grid quantiles errors on negligible mass", {
  expect_error(
    build_time_grid(
      cdf = function(x) 0, qf = function(p) Inf, rf = NULL,
      n_steps = 10, time_horizon = 1, method = "quantiles"
    ),
    "negligible mass"
  )
})

test_that("build_time_grid order_statistics returns sorted grid", {
  set.seed(42)
  d <- dist_exponential(rate = 1)
  grid <- build_time_grid(
    cdf = NULL, qf = NULL, rf = d$rf,
    n_steps = 50, time_horizon = 3, method = "order_statistics"
  )
  expect_equal(grid[1], 0)
  expect_true(all(diff(grid) > 0))
  expect_true(all(grid <= 3))
})

test_that("build_time_grid order_statistics is reproducible with set.seed", {
  d <- dist_exponential(rate = 1)
  set.seed(1)
  g1 <- build_time_grid(
    cdf = NULL, qf = NULL, rf = d$rf,
    n_steps = 20, time_horizon = 5, method = "order_statistics"
  )
  set.seed(1)
  g2 <- build_time_grid(
    cdf = NULL, qf = NULL, rf = d$rf,
    n_steps = 20, time_horizon = 5, method = "order_statistics"
  )
  expect_equal(g1, g2)
})

test_that("build_time_grid order_statistics errors without rf", {
  expect_error(
    build_time_grid(
      cdf = NULL, qf = NULL, rf = NULL,
      n_steps = 10, time_horizon = 1, method = "order_statistics"
    ),
    "sampling function"
  )
})

test_that("build_time_grid order_statistics errors when all samples exceed horizon", {
  expect_error(
    build_time_grid(
      cdf = NULL, qf = NULL, rf = function(n) rep(1e6, n),
      n_steps = 10, time_horizon = 1, method = "order_statistics"
    ),
    "exceed"
  )
})

test_that("build_time_grid order_statistics grid length <= n_steps + 1", {
  set.seed(42)
  d <- dist_exponential(rate = 1)
  grid <- build_time_grid(
    cdf = NULL, qf = NULL, rf = d$rf,
    n_steps = 50, time_horizon = 3, method = "order_statistics"
  )
  expect_true(length(grid) <= 51)
})

test_that("build_time_grid order_statistics filters to time_horizon", {
  set.seed(1)
  d <- dist_exponential(rate = 0.5)
  grid <- build_time_grid(
    cdf = NULL, qf = NULL, rf = d$rf,
    n_steps = 100, time_horizon = 1, method = "order_statistics"
  )
  expect_true(length(grid) < 102)
  expect_true(all(grid <= 1))
})
