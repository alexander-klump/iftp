test_that("save_result round-trip with iftp_result", {
  b <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  save_result(b, tmp, metadata = list(note = "test"))
  loaded <- load_result(tmp)
  expect_s3_class(loaded, "iftp_result")
  expect_equal(loaded$boundary, b$boundary)
  expect_equal(loaded$params, b$params)
  meta <- attr(loaded, "save_metadata")
  expect_equal(meta$note, "test")
  expect_true(!is.null(meta$timestamp))
  expect_true(!is.null(meta$package_version))
})

test_that("save_result round-trip with ps_result", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  save_result(result, tmp)
  loaded <- load_result(tmp)
  expect_s3_class(loaded, "ps_result")
  expect_equal(loaded$positions, result$positions)
  expect_equal(loaded$times, result$times)
  meta <- attr(loaded, "save_metadata")
  expect_true(!is.null(meta$timestamp))
})

test_that("save_result validates inputs", {
  expect_error(save_result("bad", tempfile()), "iftp_result.*ps_result")
  expect_error(save_result(data.frame(x = 1), tempfile()), "iftp_result.*ps_result")
})

test_that("load_result validates file", {
  expect_error(load_result("nonexistent.rds"), "not found")
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(list(wrong = "structure"), tmp)
  expect_error(load_result(tmp))
})
