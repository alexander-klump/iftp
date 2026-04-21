test_that("init_segments creates segment tracker without max_segments", {
  segs <- init_segments(total_rows = 10, initial_values = c(1, 2, 3))
  expect_equal(segs$idx, 3L)
  expect_equal(segs$total_rows, 10)
  expect_equal(segs$cols[[1]][1], 1)
  expect_equal(segs$cols[[2]][1], 2)
  expect_equal(segs$cols[[3]][1], 3)
})

test_that("end_and_append_segment grows capacity when full", {
  segs <- init_segments(total_rows = 5, initial_values = c(1, 2))
  # Fill to capacity by appending segments
  capacity <- length(segs$cols)
  for (i in seq_len(capacity)) {
    segs <- end_and_append_segment(segs, 1L, 2L, pre_value = 0, post_value = 0)
  }
  # Should have grown beyond initial capacity
  expect_true(length(segs$cols) > capacity)
  expect_equal(segs$idx, 2L + capacity)
})

test_that("finalize_segments trims to used columns", {
  segs <- init_segments(total_rows = 3, initial_values = c(1, 2))
  mat <- finalize_segments(segs)
  expect_equal(ncol(mat), 2)
  expect_equal(nrow(mat), 3)
})
