test_that("nbmp_simulate returns ps_result with correct fields", {
  result <- nbmp_simulate(n_particles = 10, time_horizon = 1, seed = 42)
  expect_s3_class(result, "ps_result")
  expect_equal(result$type, "nbmp")
  expect_true(is.numeric(result$times))
  expect_true(is.logical(result$is_event))
  expect_equal(length(result$times), length(result$is_event))
  expect_true(is.matrix(result$positions))
  expect_equal(nrow(result$positions), length(result$times))
  expect_false(result$is_event[1])
})

test_that("nbmp_simulate returns correct positions structure", {
  result <- nbmp_simulate(n_particles = 20, time_horizon = 1, seed = 42)
  expect_s3_class(result, "ps_result")
  expect_true(is.matrix(result$positions))
  n_rows <- length(result$times)
  expect_equal(nrow(result$positions), n_rows)
  n_events <- sum(result$is_event)
  expect_equal(ncol(result$positions), 20L + n_events)
  expect_equal(result$times[1], 0)
  expect_equal(max(result$times), 1, tolerance = 1e-10)
  expect_false(result$is_event[1])
  # Final row is propagation to time_horizon (no event)
  expect_false(result$is_event[n_rows])
  expect_true("cost" %in% names(result$params))
  expect_true("process" %in% names(result$params))
  expect_true("init" %in% names(result$params))
})

test_that("nbmp_simulate is reproducible with seed", {
  r1 <- nbmp_simulate(n_particles = 20, time_horizon = 1, seed = 42)
  r2 <- nbmp_simulate(n_particles = 20, time_horizon = 1, seed = 42)
  expect_equal(r1, r2)
})

test_that("nbmp_simulate alive count is N at non-events, N+1 at events", {
  result <- nbmp_simulate(n_particles = 10, time_horizon = 1, seed = 1)
  counts <- apply(result$positions, 1, function(row) sum(!is.na(row)))
  event_rows <- which(result$is_event)
  non_event_rows <- which(!result$is_event)
  expect_true(all(counts[non_event_rows] == 10))
  expect_true(all(counts[event_rows] == 11))
})

test_that("nbmp_simulate validates inputs", {
  expect_error(nbmp_simulate(n_particles = 1, time_horizon = 1))
  expect_error(nbmp_simulate(n_particles = 10, time_horizon = -1))
  expect_error(nbmp_simulate(n_particles = 10, time_horizon = 1, branching_rate = 0))
  expect_error(nbmp_simulate(
    n_particles = 10, time_horizon = 1,
    process = "not_a_function"
  ))
  expect_error(nbmp_simulate(n_particles = 10, time_horizon = 1, init = "bad"))
  expect_error(nbmp_simulate(n_particles = 10, time_horizon = 1, cost = "bad"))
})

test_that("nbmp_simulate first and last rows are non-events", {
  result <- nbmp_simulate(n_particles = 20, time_horizon = 1, seed = 42)
  n <- length(result$times)
  expect_false(result$is_event[1])
  expect_false(result$is_event[n])
})

test_that("nbmp_simulate path_resolution: events at correct row indices", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 1, seed = 42,
    path_resolution = 0.05
  )
  r_coarse <- nbmp_simulate(n_particles = 10, time_horizon = 1, seed = 42)
  # Event times must match between coarse and fine
  event_rows <- which(result$is_event)
  event_times_fine <- result$times[event_rows]
  event_rows_coarse <- which(r_coarse$is_event)
  event_times_coarse <- r_coarse$times[event_rows_coarse]
  expect_equal(event_times_fine, event_times_coarse, tolerance = 1e-10)
})

test_that("nbmp_simulate default cost is identity", {
  result <- nbmp_simulate(n_particles = 20, time_horizon = 1, seed = 42)
  expect_identical(result$params$cost, identity)
})

test_that("nbmp_simulate cost = identity vs cost = abs differ", {
  r_id <- nbmp_simulate(n_particles = 20, time_horizon = 1, cost = identity, seed = 42)
  r_abs <- nbmp_simulate(n_particles = 20, time_horizon = 1, cost = abs, seed = 42)
  expect_false(identical(r_id$positions, r_abs$positions))
})

test_that("nbmp_simulate works with custom process", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 0.5,
    process = process_bm(sigma = 2), seed = 1
  )
  expect_s3_class(result, "ps_result")
})

test_that("nbmp_simulate supports custom init", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 0.5,
    init = function(n) seq_len(n), seed = 1
  )
  expect_equal(result$positions[1, seq_len(10)], 1:10)
})

test_that("nbmp_simulate handles zero events", {
  result <- nbmp_simulate(
    n_particles = 2, time_horizon = 0.001, branching_rate = 0.001,
    seed = 42
  )
  expect_s3_class(result, "ps_result")
  # Initial row + propagation to time_horizon
  expect_equal(length(result$times), 2L)
  expect_equal(result$times[1], 0)
  expect_equal(result$times[2], 0.001, tolerance = 1e-10)
  expect_false(result$is_event[1])
  expect_false(result$is_event[2])
})

test_that("print.ps_result works for nbmp", {
  result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 1)
  expect_output(print(result), "N-BMP simulation")
})

test_that("nbmp_simulate killed column stores death position at event row", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.5, seed = 123)
  event_rows <- which(result$is_event)
  if (length(event_rows) > 0) {
    first_event <- event_rows[1]
    # At event: N+1 non-NA (death position + offspring)
    n_alive <- sum(!is.na(result$positions[first_event, ]))
    expect_equal(n_alive, result$params$n_particles + 1L)
    # One column was born at this row (first non-NA here, NA before)
    if (first_event > 1L) {
      born_at_event <- is.na(result$positions[first_event - 1L, ]) &
        !is.na(result$positions[first_event, ])
      expect_equal(sum(born_at_event), 1L)
    }
  }
})

# --- path_resolution ---

test_that("nbmp_simulate with path_resolution produces more time points", {
  r_coarse <- nbmp_simulate(n_particles = 10, time_horizon = 1, seed = 42)
  r_fine <- nbmp_simulate(
    n_particles = 10, time_horizon = 1, seed = 42,
    path_resolution = 0.01
  )
  expect_true(length(r_fine$times) > length(r_coarse$times))
})

test_that("nbmp_simulate path_resolution: all fields same length", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 1, seed = 42,
    path_resolution = 0.01
  )
  n <- length(result$times)
  expect_equal(length(result$is_event), n)
  expect_equal(nrow(result$positions), n)
})

test_that("nbmp_simulate path_resolution: is_event FALSE at intermediate steps", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 1, seed = 42,
    path_resolution = 0.01
  )
  n_events <- sum(result$is_event)
  n_total <- length(result$times)
  expect_true(n_total > n_events + 1L)
  # Time 0 is not an event
  expect_false(result$is_event[1])
})

test_that("nbmp_simulate path_resolution: event count unchanged", {
  r_coarse <- nbmp_simulate(n_particles = 10, time_horizon = 1, seed = 42)
  r_fine <- nbmp_simulate(
    n_particles = 10, time_horizon = 1, seed = 42,
    path_resolution = 0.01
  )
  n_events_coarse <- sum(r_coarse$is_event)
  n_events_fine <- sum(r_fine$is_event)
  expect_equal(n_events_coarse, n_events_fine)
})

test_that("nbmp_simulate path_resolution: times are monotonically increasing", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 1, seed = 42,
    path_resolution = 0.01
  )
  expect_true(all(diff(result$times) > 0))
})

test_that("nbmp_simulate path_resolution stores in params", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 1, seed = 42,
    path_resolution = 0.01
  )
  expect_equal(result$params$path_resolution, 0.01)
})

test_that("nbmp_simulate path_resolution validates input", {
  expect_error(
    nbmp_simulate(n_particles = 10, time_horizon = 1, path_resolution = -1),
    "positive"
  )
  expect_error(
    nbmp_simulate(n_particles = 10, time_horizon = 1, path_resolution = "bad"),
    "positive"
  )
})

test_that("nbmp_simulate path_resolution with zero events", {
  result <- nbmp_simulate(
    n_particles = 2, time_horizon = 0.001, branching_rate = 0.001,
    seed = 42, path_resolution = 0.0001
  )
  expect_s3_class(result, "ps_result")
  # Initial row + sub-stepped propagation to time_horizon
  expect_true(length(result$times) >= 2L)
  expect_equal(result$times[1], 0)
  expect_equal(max(result$times), 0.001, tolerance = 1e-10)
})

test_that("nbmp_simulate always reaches time_horizon", {
  for (s in 1:5) {
    result <- nbmp_simulate(n_particles = 5, time_horizon = 2.5, seed = s)
    expect_equal(max(result$times), 2.5, tolerance = 1e-10)
  }
})

test_that("nbmp_simulate event count is random (Poisson)", {
  counts <- vapply(1:20, function(s) {
    r <- nbmp_simulate(n_particles = 10, time_horizon = 1, seed = s)
    sum(r$is_event)
  }, integer(1))
  # Not all identical (would be if deterministic)
  expect_true(length(unique(counts)) > 1L)
  # Mean should be near n_particles * branching_rate * time_horizon = 10
  expect_true(abs(mean(counts) - 10) < 5)
})

test_that("print.ps_result shows time points with path_resolution", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 1, seed = 42,
    path_resolution = 0.01
  )
  expect_output(print(result), "time points")
})

test_that("nbmp_simulate kills particle with max cost (identity)", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 1,
    cost = identity, seed = 42
  )
  event_rows <- which(result$is_event)
  expect_true(length(event_rows) > 0)

  for (ev in event_rows) {
    if (ev <= 1L || ev >= nrow(result$positions)) next

    pre <- result$positions[ev - 1L, ]
    post_row <- ev + 1L
    post <- result$positions[post_row, ]

    alive_pre <- which(!is.na(pre))
    killed_cols <- alive_pre[is.na(post[alive_pre])]
    if (length(killed_cols) != 1L) next

    alive_positions <- pre[alive_pre]
    costs <- identity(alive_positions)
    # Skip ties: if multiple particles share the maximum cost, any could be killed
    if (sum(costs == max(costs)) > 1L) next

    expected_killed <- alive_pre[which.max(costs)]
    expect_equal(killed_cols, expected_killed)
  }
})

test_that("nbmp_simulate kills particle with max cost (abs)", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 1,
    cost = abs, seed = 42
  )
  event_rows <- which(result$is_event)
  expect_true(length(event_rows) > 0)

  for (ev in event_rows) {
    if (ev <= 1L || ev >= nrow(result$positions)) next

    pre <- result$positions[ev - 1L, ]
    post_row <- ev + 1L
    post <- result$positions[post_row, ]

    alive_pre <- which(!is.na(pre))
    killed_cols <- alive_pre[is.na(post[alive_pre])]
    if (length(killed_cols) != 1L) next

    alive_positions <- pre[alive_pre]
    costs <- abs(alive_positions)
    # Skip ties: if multiple particles share the maximum cost, any could be killed
    if (sum(costs == max(costs)) > 1L) next

    expected_killed <- alive_pre[which.max(costs)]
    expect_equal(killed_cols, expected_killed)
  }
})

test_that("nbmp_simulate aborts on NaN from process", {
  bad_process <- function(x, dt) rep(NaN, length(x))
  class(bad_process) <- "iftp_process"
  attr(bad_process, "process_type") <- "bad"
  expect_error(
    nbmp_simulate(
      n_particles = 10, time_horizon = 1,
      process = bad_process, seed = 1
    ),
    "non-finite"
  )
})

# --- jump-segment breaks ---

test_that("nbmp_simulate with BM+Poisson creates jump segments", {
  result <- nbmp_simulate(
    n_particles = 10, time_horizon = 1,
    process = process_bm_drift_poisson(poisson_intensity = 10),
    cost = identity, seed = 42
  )
  expect_s3_class(result, "ps_result")
  n_events <- sum(result$is_event)
  # With jump segments, ncol > n_particles + n_events
  expect_true(ncol(result$positions) > 10L + n_events)
})

test_that("nbmp_simulate jump segments show position discontinuity", {
  # Use high Poisson intensity to guarantee jumps
  result <- nbmp_simulate(
    n_particles = 5, time_horizon = 0.5,
    process = process_bm_drift_poisson(sigma = 0.001, poisson_intensity = 50),
    cost = identity, seed = 42, path_resolution = 0.01
  )
  # Find a non-event row where a segment ends and another begins
  # (= jump segment break). At such a row, there should be more than
  # n_particles non-NA values (old segment end + new segment start).
  non_event_rows <- which(!result$is_event)
  # Exclude row 1
  non_event_rows <- non_event_rows[non_event_rows > 1L]
  counts <- apply(
    result$positions[non_event_rows, , drop = FALSE], 1,
    function(r) sum(!is.na(r))
  )
  # Some non-event rows should have > 5 non-NA (jump breaks)
  expect_true(any(counts > 5L))
})

test_that("nbmp_simulate without jumps unchanged column count", {
  result <- nbmp_simulate(n_particles = 10, time_horizon = 1, seed = 42)
  n_events <- sum(result$is_event)
  # BM has no jumps, so column count = n_particles + n_events (unchanged)
  expect_equal(ncol(result$positions), 10L + n_events)
})
