# --- iftp_result export ---

test_that("export_result returns boundary data frame for iftp_result", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 20,
    n_particles = 100, seed = 1
  )
  df <- export_result(result)
  expect_s3_class(df, "data.frame")
  expect_named(df, c("time", "boundary"))
  expect_equal(nrow(df), nrow(result$boundary))
})

# --- ps_result export (nbmp) ---

test_that("export_result returns long-format data frame for nbmp ps_result", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.5, seed = 42)
  paths <- export_result(result)
  expect_s3_class(paths, "data.frame")
  expect_named(paths, c("particle_id", "time", "position", "is_alive"))
  n_cols <- ncol(result$positions)
  n_times <- length(result$times)
  expect_equal(nrow(paths), n_cols * n_times)
})

test_that("export_result has correct particle_id and time structure", {
  result <- nbmp_simulate(n_particles = 3, time_horizon = 0.3, seed = 1)
  paths <- export_result(result)
  n_times <- length(result$times)
  n_cols <- ncol(result$positions)
  expect_equal(as.integer(table(paths$particle_id)), rep(n_times, n_cols))
  for (pid in seq_len(n_cols)) {
    sub <- paths[paths$particle_id == pid, ]
    expect_equal(sub$time, result$times)
  }
})

test_that("export_result nbmp has dead lineage segments (is_alive = FALSE)", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.5, seed = 42)
  paths <- export_result(result)
  expect_true(any(!paths$is_alive))
})

# --- ps_result export (qtps) ---

test_that("export_result works with qtps ps_result", {
  result <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  paths <- export_result(result)
  expect_s3_class(paths, "data.frame")
  expect_named(paths, c("particle_id", "time", "position", "is_alive"))
  n_times <- length(result$times)
  expect_equal(nrow(paths), 10 * n_times)
})

test_that("export_result QTPS: is_alive transitions to FALSE", {
  result <- qtps_simulate(
    n_particles = 5, cdf = dist_exponential(), time_horizon = 5, seed = 42
  )
  paths <- export_result(result)
  expect_true(any(!paths$is_alive))
  for (pid in 1:5) {
    sub <- paths[paths$particle_id == pid, ]
    alive_vals <- sub$is_alive
    first_dead <- which(!alive_vals)[1]
    if (!is.na(first_dead)) {
      expect_true(all(!alive_vals[first_dead:length(alive_vals)]))
    }
  }
})

# --- File export ---

test_that("export_result writes CSV", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  export_result(result, file = tmp)
  expect_true(file.exists(tmp))
  loaded <- utils::read.csv(tmp)
  expect_named(loaded, c("particle_id", "time", "position", "is_alive"))
})

test_that("export_result writes CSV for iftp_result", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  export_result(result, file = tmp)
  expect_true(file.exists(tmp))
  loaded <- utils::read.csv(tmp)
  expect_named(loaded, c("time", "boundary"))
})

test_that("export_result ps_result JSON requires format", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  expect_error(
    export_result(result, file = tempfile(fileext = ".json")),
    "requires.*format"
  )
})

test_that("export_result writes JSON for iftp_result", {
  skip_if_not_installed("jsonlite")
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  export_result(result, file = tmp)
  expect_true(file.exists(tmp))
  loaded <- jsonlite::fromJSON(tmp)
  expect_named(loaded, c("time", "boundary"))
})

test_that("export_result rejects bad file extension", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  expect_error(export_result(result, file = "test.xyz"), "extension")
})

# --- Validation ---

test_that("export_result validates input", {
  expect_error(export_result("not_a_result"), "iftp_result.*ps_result")
})

# --- Matrix format ---

test_that("export_result matrix format returns correct structure", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.5, seed = 42)
  mat <- export_result(result, format = "matrix")
  expect_type(mat, "list")
  expect_named(mat, c("times", "positions"))
  expect_equal(mat$times, result$times)
  expect_length(mat$positions, nrow(result$positions))
  # Each row has same number of elements as columns in positions matrix

  for (i in seq_along(mat$positions)) {
    expect_length(mat$positions[[i]], ncol(result$positions))
  }
})

test_that("export_result matrix format preserves values", {
  result <- nbmp_simulate(n_particles = 3, time_horizon = 0.2, seed = 1)
  mat <- export_result(result, format = "matrix")
  # First row should match positions[1, ]
  expect_equal(mat$positions[[1]], as.numeric(result$positions[1, ]))
  # Last row should match positions[nrow, ]
  n <- nrow(result$positions)
  expect_equal(mat$positions[[n]], as.numeric(result$positions[n, ]))
})

test_that("export_result matrix format preserves NAs", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.5, seed = 42)
  mat <- export_result(result, format = "matrix")
  # Count total NAs in original matrix
  orig_na <- sum(is.na(result$positions))
  # Count NAs in reconstructed rows
  mat_na <- sum(vapply(mat$positions, function(r) sum(is.na(r)), integer(1)))
  expect_equal(mat_na, orig_na)
})

test_that("export_result matrix format writes JSON", {
  skip_if_not_installed("jsonlite")
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  export_result(result, file = tmp, format = "matrix")
  expect_true(file.exists(tmp))
  loaded <- jsonlite::fromJSON(tmp)
  expect_true("times" %in% names(loaded))
  expect_true("positions" %in% names(loaded))
})

test_that("export_result matrix format rejects CSV", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  expect_error(
    export_result(result, file = "test.csv", format = "matrix"),
    "requires JSON"
  )
})

test_that("export_result matrix format works for QTPS", {
  result <- qtps_simulate(
    n_particles = 5, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  mat <- export_result(result, format = "matrix")
  expect_named(mat, c("times", "positions"))
  expect_equal(mat$times, result$times)
  expect_length(mat$positions, length(result$times))
})

# --- Segments format ---

test_that("export_result segments format returns correct structure", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.5, seed = 42)
  segs <- export_result(result, format = "segments")
  expect_type(segs, "list")
  expect_length(segs, ncol(result$positions))
  for (seg in segs) {
    expect_true(all(c("id", "times", "positions") %in% names(seg)))
    expect_type(seg$id, "integer")
    expect_type(seg$times, "double")
    expect_type(seg$positions, "double")
    expect_equal(length(seg$times), length(seg$positions))
  }
})

test_that("export_result segments contain only non-NA values", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.5, seed = 42)
  segs <- export_result(result, format = "segments")
  for (seg in segs) {
    expect_false(any(is.na(seg$positions)))
    expect_false(any(is.na(seg$times)))
  }
})

test_that("export_result segments ids are sequential", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.5, seed = 42)
  segs <- export_result(result, format = "segments")
  ids <- vapply(segs, function(s) s$id, integer(1))
  expect_equal(ids, seq_len(ncol(result$positions)))
})

test_that("export_result segments times are subset of result$times", {
  result <- nbmp_simulate(n_particles = 3, time_horizon = 0.3, seed = 1)
  segs <- export_result(result, format = "segments")
  for (seg in segs) {
    expect_true(all(seg$times %in% result$times))
  }
})

test_that("export_result segments positions match matrix values", {
  result <- nbmp_simulate(n_particles = 3, time_horizon = 0.2, seed = 1)
  segs <- export_result(result, format = "segments")
  for (seg in segs) {
    col <- result$positions[, seg$id]
    alive <- which(!is.na(col))
    expect_equal(seg$positions, col[alive])
    expect_equal(seg$times, result$times[alive])
  }
})

test_that("export_result segments format writes JSON", {
  skip_if_not_installed("jsonlite")
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  export_result(result, file = tmp, format = "segments")
  expect_true(file.exists(tmp))
  loaded <- jsonlite::fromJSON(tmp)
  # jsonlite reads array-of-objects as data frame
  expect_true("id" %in% names(loaded))
  expect_true("times" %in% names(loaded))
  expect_true("positions" %in% names(loaded))
})

test_that("export_result segments format rejects CSV", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  expect_error(
    export_result(result, file = "test.csv", format = "segments"),
    "requires JSON"
  )
})

test_that("export_result segments format works for QTPS", {
  result <- qtps_simulate(
    n_particles = 5, cdf = dist_exponential(), time_horizon = 2, seed = 42
  )
  segs <- export_result(result, format = "segments")
  expect_length(segs, 5)
  for (seg in segs) {
    expect_true(length(seg$positions) > 0)
    expect_false(any(is.na(seg$positions)))
  }
})

# --- iftp_result ignores format ---

test_that("export_result ignores format for iftp_result", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  df1 <- export_result(result)
  # format is accepted but ignored for iftp_result
  df2 <- export_result(result, format = "matrix")
  expect_equal(df1, df2)
})

# --- suggest_filename ---

test_that("suggest_filename returns string with correct extension", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  fn <- suggest_filename(result, "csv")
  expect_type(fn, "character")
  expect_match(fn, "\\.csv$")

  fn_json <- suggest_filename(result, "json")
  expect_match(fn_json, "\\.json$")
})

test_that("suggest_filename includes algorithm and distribution for iftp_result", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  fn <- suggest_filename(result, "csv")
  expect_match(fn, "^anulova_exponential_")
  expect_match(fn, "n100")
  expect_match(fn, "t1")
  expect_match(fn, "steps10")
  expect_match(fn, "s1")
})

test_that("suggest_filename includes algorithm-specific params", {
  result_sk <- iftp_soft_killing(dist_exponential(),
    time_horizon = 1,
    killing_rate = 2, time_steps = 10,
    n_particles = 100, seed = 1
  )
  fn <- suggest_filename(result_sk, "csv")
  expect_match(fn, "^soft_killing_exponential_")
  expect_match(fn, "kr2")

  result_q <- iftp_quantile(dist_exponential(),
    time_horizon = 2, time_steps = 5,
    remove_per_step = 1, seed = 1
  )
  fn_q <- suggest_filename(result_q, "csv")
  expect_match(fn_q, "^quantile_exponential_")
  expect_match(fn_q, "rm1")
})

test_that("suggest_filename works for nbmp ps_result", {
  result <- nbmp_simulate(n_particles = 10, time_horizon = 0.5, seed = 42)
  fn <- suggest_filename(result, "csv")
  expect_match(fn, "^nbmp_")
  expect_match(fn, "n10")
  expect_match(fn, "t0\\.5")
  expect_match(fn, "br1")
})

test_that("suggest_filename works for qtps ps_result", {
  result <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(),
    time_horizon = 2, seed = 42
  )
  fn <- suggest_filename(result, "csv")
  expect_match(fn, "^qtps_exponential_")
  expect_match(fn, "n10")
  expect_match(fn, "t2")
})

test_that("suggest_filename includes timestamp", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  fn <- suggest_filename(result, "csv")
  # Timestamp pattern: YYYYMMDD_HHMMSS
  expect_match(fn, "\\d{8}_\\d{6}")
})

test_that("suggest_filename appends suffix before timestamp", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 42)
  fn <- suggest_filename(result, "json", suffix = "segments")
  expect_match(fn, "segments_\\d{8}_\\d{6}\\.json$")
})

test_that("suggest_filename handles custom CDF (no distribution name)", {
  result <- iftp_anulova(pexp,
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  fn <- suggest_filename(result, "csv")
  expect_match(fn, "^anulova_n100")
  expect_no_match(fn, "anulova__")
})

test_that("suggest_filename sanitizes distribution names", {
  result <- iftp_soft_killing(
    dist_soft_killing_constant_barrier(1, 1),
    time_horizon = 1, time_steps = 10, n_particles = 100, seed = 1
  )
  fn <- suggest_filename(result, "csv")
  # Spaces/parens should become hyphens
  expect_match(fn, "soft-killing-constant-barrier")
  expect_no_match(fn, " ")
  expect_no_match(fn, "\\(")
})

test_that("suggest_filename validates input", {
  expect_error(suggest_filename("not_a_result"), "iftp_result.*ps_result")
})

# --- capture_seed ---

test_that("capture_seed generates and stores seed when seed is NULL", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, capture_seed = TRUE
  )
  expect_true(!is.null(result$params$seed))
  expect_type(result$params$seed, "integer")

  # The captured seed reproduces the same result
  result2 <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = result$params$seed
  )
  expect_equal(result$boundary, result2$boundary)
})

test_that("explicit seed is stored in params regardless of capture_seed", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 42
  )
  expect_equal(result$params$seed, 42)

  result2 <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, capture_seed = TRUE, seed = 99
  )
  expect_equal(result2$params$seed, 99)
})

test_that("seed is NULL in params when capture_seed is FALSE and no seed given", {
  result <- nbmp_simulate(n_particles = 5, time_horizon = 0.3)
  expect_null(result$params$seed)
})

test_that("capture_seed works for all simulation functions", {
  r1 <- iftp_quantile(dist_exponential(),
    time_horizon = 1, time_steps = 5,
    remove_per_step = 1, capture_seed = TRUE
  )
  expect_true(!is.null(r1$params$seed))

  r2 <- iftp_soft_killing(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, capture_seed = TRUE
  )
  expect_true(!is.null(r2$params$seed))

  r3 <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, capture_seed = TRUE)
  expect_true(!is.null(r3$params$seed))

  r4 <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(),
    time_horizon = 1, capture_seed = TRUE
  )
  expect_true(!is.null(r4$params$seed))
})

# --- suggest_filename: extended parameters ---

test_that("suggest_filename includes seed when stored", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 42
  )
  fn <- suggest_filename(result, "csv")
  expect_match(fn, "s42")
})

test_that("suggest_filename omits seed when not stored", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100
  )
  fn <- suggest_filename(result, "csv")
  expect_no_match(fn, "_s\\d+_")
})

test_that("suggest_filename includes non-default process", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, process = process_ou(), seed = 1
  )
  fn <- suggest_filename(result, "csv")
  expect_match(fn, "ou")
})

test_that("suggest_filename omits default bm process", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  fn <- suggest_filename(result, "csv")
  expect_no_match(fn, "_bm_")
})

test_that("suggest_filename includes non-default cost", {
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, cost = identity, seed = 1
  )
  fn <- suggest_filename(result, "csv")
  expect_match(fn, "cost-identity")
})

test_that("suggest_filename omits default cost", {
  # abs is default for anulova
  result <- iftp_anulova(dist_exponential(),
    time_horizon = 1, time_steps = 10,
    n_particles = 100, seed = 1
  )
  fn <- suggest_filename(result, "csv")
  expect_no_match(fn, "cost-")

  # identity is default for nbmp
  result2 <- nbmp_simulate(n_particles = 5, time_horizon = 0.3, seed = 1)
  fn2 <- suggest_filename(result2, "csv")
  expect_no_match(fn2, "cost-")
})

test_that("suggest_filename includes path_resolution for PS", {
  result <- nbmp_simulate(
    n_particles = 5, time_horizon = 0.3,
    path_resolution = 0.01, seed = 1
  )
  fn <- suggest_filename(result, "csv")
  expect_match(fn, "res0\\.01")
})

test_that("suggest_filename includes non-default method", {
  result <- qtps_simulate(
    n_particles = 10, cdf = dist_exponential(),
    time_horizon = 1, method = "order_statistics", seed = 1
  )
  fn <- suggest_filename(result, "csv")
  expect_match(fn, "order_statistics")
})
