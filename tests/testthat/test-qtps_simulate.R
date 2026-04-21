test_that("qtps_simulate returns ps_result with correct fields", {
  result <- qtps_simulate(
    n_particles = 50, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  expect_s3_class(result, "ps_result")
  expect_equal(result$type, "qtps")
  expect_true(is.numeric(result$times))
  expect_true(is.logical(result$is_event))
  expect_true(is.matrix(result$positions))
  expect_equal(nrow(result$positions), length(result$times))
  expect_true("cost" %in% names(result$params))
  expect_true("process" %in% names(result$params))
  expect_true("init" %in% names(result$params))
})

test_that("qtps_simulate time grid has fewer steps than particles", {
  result <- qtps_simulate(
    n_particles = 50, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  n_steps <- length(result$times) - 1L
  expect_true(n_steps > 0)
  expect_true(n_steps < 50)
})

test_that("qtps_simulate is reproducible with seed", {
  r1 <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(),
    time_horizon = 1, seed = 42
  )
  r2 <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(),
    time_horizon = 1, seed = 42
  )
  expect_equal(r1, r2)
})

test_that("qtps_simulate returns positions matrix", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  expect_true(is.matrix(result$positions))
  expect_equal(ncol(result$positions), 20)
  expect_equal(nrow(result$positions), length(result$times))
})

test_that("qtps_simulate works with cost = identity", {
  result <- qtps_simulate(
    n_particles = 50, cdf = dist_exponential(), time_horizon = 2,
    cost = identity, seed = 42
  )
  expect_s3_class(result, "ps_result")
})

test_that("qtps_simulate dead particles become NA", {
  result <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(), time_horizon = 5, seed = 42
  )
  # Last row is an event row: killed particle's death position is preserved,
  # so we see the survivor + the just-killed particle (2 non-NA).
  last_row <- result$positions[nrow(result$positions), ]
  n_events <- sum(result$is_event)
  expect_equal(sum(!is.na(last_row)), 10L - n_events + 1L)

  # Non-event rows (if any) after a kill show only survivors
  # Verify with path_resolution to get intermediate rows
  result2 <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(), time_horizon = 5,
    path_resolution = 0.01, seed = 42
  )
  # Find a non-event row right after the last event
  event_rows <- which(result2$is_event)
  last_event <- event_rows[length(event_rows)]
  if (last_event < nrow(result2$positions)) {
    after_last_event <- result2$positions[last_event + 1L, ]
    n_survivors <- 10L - length(event_rows)
    expect_equal(sum(!is.na(after_last_event)), n_survivors)
  }
})

test_that("qtps_simulate validates inputs", {
  expect_error(qtps_simulate(
    n_particles = 1, cdf = dist_exponential(),
    time_horizon = 1
  ))
  expect_error(qtps_simulate(
    n_particles = 50, cdf = "bad",
    time_horizon = 1
  ))
  expect_error(qtps_simulate(
    n_particles = 50, cdf = dist_exponential(),
    time_horizon = -1
  ))
  expect_error(qtps_simulate(
    n_particles = 50,
    cdf = function(x) pexp(x), time_horizon = 1
  ))
  expect_error(qtps_simulate(
    n_particles = 50, cdf = dist_exponential(),
    time_horizon = 1, cost = "bad"
  ))
})

test_that("qtps_simulate accepts iftp_dist objects", {
  result <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(), time_horizon = 1, seed = 42
  )
  expect_s3_class(result, "ps_result")
})

test_that("print.ps_result works for qtps", {
  result <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(), time_horizon = 1, seed = 42
  )
  expect_output(print(result), "QTPS")
})

test_that("qtps_simulate has is_event vector", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  expect_true(!is.null(result$is_event))
  expect_true(is.logical(result$is_event))
  expect_equal(length(result$is_event), length(result$times))
  # Time 0 is not an event
  expect_false(result$is_event[1])
  # Without path_resolution, all other steps are events
  expect_true(all(result$is_event[-1]))
})

test_that("qtps_simulate is_event count equals deletion count", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  n_steps <- length(result$times) - 1L
  expect_equal(sum(result$is_event), n_steps)
})

# --- path_resolution ---

test_that("qtps_simulate with path_resolution produces more time points", {
  r_coarse <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  r_fine <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42,
    path_resolution = 0.01
  )
  expect_true(length(r_fine$times) > length(r_coarse$times))
})

test_that("qtps_simulate path_resolution: is_event marks only original steps", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42,
    path_resolution = 0.01
  )
  n_events <- sum(result$is_event)
  n_total <- length(result$times)
  expect_true(n_total > n_events + 1L)
})

test_that("qtps_simulate path_resolution with positions", {
  result <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(), time_horizon = 2,
    path_resolution = 0.05, seed = 42
  )
  expect_true(is.matrix(result$positions))
  expect_equal(nrow(result$positions), length(result$times))
  expect_equal(ncol(result$positions), 10)
})

test_that("qtps_simulate path_resolution: times monotonically increasing", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42,
    path_resolution = 0.01
  )
  expect_true(all(diff(result$times) > 0))
})

test_that("qtps_simulate path_resolution stores in params", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42,
    path_resolution = 0.05
  )
  expect_equal(result$params$path_resolution, 0.05)
})

test_that("qtps_simulate path_resolution validates input", {
  expect_error(
    qtps_simulate(
      n_particles = 20, cdf = dist_exponential(),
      time_horizon = 2, path_resolution = -1
    ),
    "positive"
  )
})

test_that("print.ps_result shows time points with path_resolution for qtps", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42,
    path_resolution = 0.01
  )
  expect_output(print(result), "time points")
})

test_that("qtps_simulate works with custom process", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2,
    process = process_bm(sigma = 2), seed = 42
  )
  expect_s3_class(result, "ps_result")
})

test_that("qtps_simulate works with BM+Poisson process", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2,
    process = process_bm_drift_poisson(), cost = identity, seed = 42
  )
  expect_s3_class(result, "ps_result")
})

test_that("qtps_simulate supports custom init", {
  n <- 20L
  result <- qtps_simulate(
    n_particles = n, cdf = dist_exponential(), time_horizon = 2,
    init = function(n) seq_len(n), seed = 42
  )
  expect_equal(result$positions[1, ], seq_len(n))
})

test_that("qtps_simulate time grid stays within time_horizon", {
  for (s in 1:5) {
    result <- qtps_simulate(
      n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = s
    )
    expect_true(max(result$times) <= 2)
  }
})

test_that("qtps_simulate kills particle with max cost (abs)", {
  result <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(), time_horizon = 5,
    cost = abs, seed = 42
  )
  event_rows <- which(result$is_event)
  expect_true(length(event_rows) > 0)

  for (ev in event_rows) {
    if (ev >= nrow(result$positions)) next

    alive_at_event <- which(!is.na(result$positions[ev, ]))
    alive_positions <- result$positions[ev, alive_at_event]

    # Skip ties — which.max picks first, simulation may pick any
    costs <- abs(alive_positions)
    if (sum(costs == max(costs)) > 1L) next

    next_row <- result$positions[ev + 1L, ]
    killed_cols <- alive_at_event[is.na(next_row[alive_at_event])]
    if (length(killed_cols) != 1L) next

    expected_killed <- alive_at_event[which.max(costs)]
    expect_equal(killed_cols, expected_killed)
  }
})

test_that("qtps_simulate kills particle with max cost (identity)", {
  result <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(), time_horizon = 5,
    cost = identity, seed = 42
  )
  event_rows <- which(result$is_event)
  expect_true(length(event_rows) > 0)

  for (ev in event_rows) {
    if (ev >= nrow(result$positions)) next

    alive_at_event <- which(!is.na(result$positions[ev, ]))
    alive_positions <- result$positions[ev, alive_at_event]

    costs <- identity(alive_positions)
    if (sum(costs == max(costs)) > 1L) next

    next_row <- result$positions[ev + 1L, ]
    killed_cols <- alive_at_event[is.na(next_row[alive_at_event])]
    if (length(killed_cols) != 1L) next

    expected_killed <- alive_at_event[which.max(costs)]
    expect_equal(killed_cols, expected_killed)
  }
})

test_that("qtps_simulate default cost is abs", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  expect_identical(result$params$cost, abs)
})

test_that("qtps_simulate aborts on NaN from process", {
  bad_process <- function(x, dt) rep(NaN, length(x))
  class(bad_process) <- "iftp_process"
  attr(bad_process, "process_type") <- "bad"
  expect_error(
    qtps_simulate(
      n_particles = 20, cdf = dist_exponential(), time_horizon = 2,
      process = bad_process, seed = 1
    ),
    "non-finite"
  )
})

test_that("qtps_simulate method = quantiles is default (unchanged behavior)", {
  r1 <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(),
    time_horizon = 2, seed = 42
  )
  r2 <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(),
    time_horizon = 2, method = "quantiles", seed = 42
  )
  expect_equal(r1, r2)
})

test_that("qtps_simulate method = order_statistics returns ps_result", {
  result <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(),
    time_horizon = 2,
    method = "order_statistics", seed = 42
  )
  expect_s3_class(result, "ps_result")
  expect_equal(result$type, "qtps")
  expect_equal(result$params$method, "order_statistics")
  expect_equal(ncol(result$positions), 30)
})

test_that("qtps_simulate order_statistics is reproducible with seed", {
  r1 <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(),
    time_horizon = 2,
    method = "order_statistics", seed = 42
  )
  r2 <- qtps_simulate(
    n_particles = 30, cdf = dist_exponential(),
    time_horizon = 2,
    method = "order_statistics", seed = 42
  )
  expect_equal(r1, r2)
})

test_that("qtps_simulate order_statistics time grid is sorted", {
  result <- qtps_simulate(
    n_particles = 50, cdf = dist_exponential(),
    time_horizon = 3,
    method = "order_statistics", seed = 1
  )
  expect_equal(result$times[1], 0)
  expect_true(all(diff(result$times) > 0))
  expect_true(all(result$times <= 3))
})

test_that("qtps_simulate order_statistics errors for dist without sampler", {
  expect_error(
    qtps_simulate(
      n_particles = 30,
      cdf = dist_soft_killing_constant_barrier(),
      time_horizon = 2, method = "order_statistics", seed = 1
    ),
    "sampling function"
  )
})

test_that("qtps_simulate order_statistics errors for raw CDF", {
  expect_error(
    qtps_simulate(
      n_particles = 30, cdf = function(x) pexp(x),
      qf = function(p) qexp(p),
      time_horizon = 2, method = "order_statistics", seed = 1
    ),
    "sampling function"
  )
})

test_that("qtps_simulate order_statistics differs from quantiles", {
  r_q <- qtps_simulate(
    n_particles = 50, cdf = dist_exponential(),
    time_horizon = 3, method = "quantiles", seed = 42
  )
  r_os <- qtps_simulate(
    n_particles = 50, cdf = dist_exponential(),
    time_horizon = 3, method = "order_statistics", seed = 42
  )
  expect_false(identical(r_q$times, r_os$times))
})

test_that("qtps_simulate order_statistics works with path_resolution", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(),
    time_horizon = 2,
    method = "order_statistics",
    path_resolution = 0.1, seed = 42
  )
  expect_s3_class(result, "ps_result")
  expect_true(sum(result$is_event) < length(result$times))
})

# --- Jump-segment tests ---

test_that("qtps_simulate with BM+Poisson creates jump segments", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2,
    process = process_bm_drift_poisson(poisson_intensity = 10),
    cost = identity, seed = 42
  )
  expect_s3_class(result, "ps_result")
  # With jump segments, ncol > n_particles
  expect_true(ncol(result$positions) > 20L)
})

test_that("qtps_simulate without jumps keeps n_particles columns", {
  result <- qtps_simulate(
    n_particles = 20, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  # BM has no jumps — column count unchanged
  expect_equal(ncol(result$positions), 20)
})

test_that("qtps_simulate jump segments show discontinuity", {
  result <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(), time_horizon = 2,
    process = process_bm_drift_poisson(sigma = 0.001, poisson_intensity = 50),
    cost = identity, path_resolution = 0.05, seed = 42
  )
  # Non-event rows with jump breaks have > n_alive non-NA values
  non_event_rows <- which(!result$is_event & seq_along(result$is_event) > 1L)
  if (length(non_event_rows) > 0) {
    counts <- apply(
      result$positions[non_event_rows, , drop = FALSE], 1,
      function(r) sum(!is.na(r))
    )
    # Some should exceed the number of alive particles at that point
    expect_true(any(counts > 1L))
  }
})
