test_that("is_iftp_result works", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 50, seed = 1
  )
  expect_true(is_iftp_result(result))
  expect_false(is_iftp_result(data.frame()))
  expect_false(is_iftp_result(NULL))
  expect_false(is_iftp_result(42))
})

test_that("is_ps_result works", {
  result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 1)
  expect_true(is_ps_result(result))
  expect_false(is_ps_result(data.frame()))
})

test_that("is_iftp_dist works", {
  d <- dist_exponential()
  expect_true(is_iftp_dist(d))
  expect_false(is_iftp_dist(pexp))
})

test_that("is_iftp_process works", {
  p <- process_bm()
  expect_true(is_iftp_process(p))
  expect_false(is_iftp_process(identity))
})
