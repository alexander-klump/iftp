# --- new_ps_result() ---

test_that("new_ps_result creates correct structure for nbmp type", {
  result <- new_ps_result(
    type = "nbmp", times = c(0, 0.5, 1.0),
    is_event = c(FALSE, TRUE, FALSE),
    positions = matrix(1:9, nrow = 3),
    params = list(n_particles = 2L, time_horizon = 1)
  )

  expect_s3_class(result, "ps_result")
  expect_equal(result$type, "nbmp")
  expect_equal(result$times, c(0, 0.5, 1.0))
  expect_equal(result$is_event, c(FALSE, TRUE, FALSE))
  expect_true(is.matrix(result$positions))
})

test_that("new_ps_result creates correct structure for qtps type", {
  result <- new_ps_result(
    type = "qtps", times = c(0, 0.5, 1.0),
    is_event = c(FALSE, TRUE, TRUE),
    positions = matrix(c(0, 0.3, 0.4, 0, -0.5, NA), nrow = 3, ncol = 2),
    params = list(n_particles = 2L, time_horizon = 1)
  )
  expect_s3_class(result, "ps_result")
  expect_equal(result$type, "qtps")
})

# --- segment-tracking helpers ---

test_that("init_segments creates pre-allocated list", {
  segs <- init_segments(total_rows = 10L)
  expect_type(segs$cols, "list")
  expect_true(length(segs$cols) >= 1L)
  expect_equal(length(segs$cols[[1L]]), 10L)
  expect_true(all(is.na(segs$cols[[1L]])))
  expect_equal(segs$idx, 1L)
})

test_that("finalize_segments returns matrix with used columns only", {
  segs <- init_segments(total_rows = 3L)
  segs$cols[[1L]] <- c(1, 2, 3)
  mat <- finalize_segments(segs)
  expect_equal(ncol(mat), 1L)
  expect_equal(nrow(mat), 3L)
  expect_equal(mat[, 1], c(1, 2, 3))
})
