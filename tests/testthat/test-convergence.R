test_that("nbmp_simulate works with n_particles = 2", {
  result <- nbmp_simulate(n_particles = 2, time_horizon = 0.5, seed = 42)
  expect_s3_class(result, "ps_result")
  expect_equal(result$params$n_particles, 2)
})

test_that("qtps_simulate works with n_particles = 2", {
  result <- qtps_simulate(
    n_particles = 2, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  expect_s3_class(result, "ps_result")
})

test_that("iftp_anulova handles F(0) > 0", {
  # dist_uniform(min = -1, max = 1) has F(0) = 0.5
  result <- iftp_anulova(
    cdf = dist_uniform(min = -1, max = 1), time_horizon = 0.5,
    time_steps = 20, n_particles = 200, seed = 42
  )
  expect_s3_class(result, "iftp_result")
  expect_true(all(is.finite(result$boundary$boundary)))
})

test_that("anulova and quantile boundaries are comparable", {
  d <- dist_exponential(rate = 1)
  res_a <- iftp_anulova(
    cdf = d, time_horizon = 1, time_steps = 50, n_particles = 5000, seed = 42
  )
  res_q <- iftp_quantile(
    cdf = d, time_horizon = 1, time_steps = 100, remove_per_step = 50, seed = 42
  )
  # At t = 0.5, both boundaries should be roughly similar (compare in cost space)
  b_a <- approx(res_a$boundary$time, abs(res_a$boundary$boundary), xout = 0.5)$y
  b_q <- approx(res_q$boundary$time, abs(res_q$boundary$boundary), xout = 0.5)$y
  # Deliberately loose tolerance: different algorithms, different grids,
  # statistical noise. This verifies rough agreement, not convergence.
  expect_lt(abs(b_a - b_q) / max(b_a, b_q), 0.5)
})

test_that("soft-killing boundary converges to constant for analytical distribution", {
  d <- dist_soft_killing_constant_barrier(barrier = 1, killing_rate = 1)
  result <- iftp_soft_killing(
    cdf = d,
    time_horizon = 3,
    killing_rate = 1,
    time_steps = 50,
    n_particles = 5000,
    seed = 42
  )
  expect_s3_class(result, "iftp_result")
  # The analytical solution has constant boundary b(t) = 1.
  # Skip first and last 3 grid points (boundary approximation is
  # less stable near t = 0 and t = T).
  boundary_vals <- result$boundary$boundary
  n <- length(boundary_vals)
  interior <- boundary_vals[4:(n - 3)]
  expect_true(all(abs(interior - 1) < 0.15),
    info = sprintf("max deviation from 1: %.3f", max(abs(interior - 1)))
  )
})
