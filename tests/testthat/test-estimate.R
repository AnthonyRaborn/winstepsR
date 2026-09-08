test_that("winsteps_estimate generates all input files without running Winsteps", {
  data <- data.frame(
    id = rep(c("001", "002"), each = 3),
    item = rep(c("A", "B", "C"), times = 2),
    score = c(1, 0, 1, 0, 1, 1)
  )

  tmp_dir <- tempfile()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  old <- getOption("winstepsR.exe_path")
  options(winstepsR.exe_path = "Winsteps.exe")
  on.exit(options(winstepsR.exe_path = old), add = TRUE)

  result <- winsteps_estimate(
    data = data, id_col = "id", item_col = "item", score_col = "score",
    items = c("A", "B", "C"), anchor_values = c(-0.5, 0, 0.5),
    run_id = "test_run", working_dir = tmp_dir, run = FALSE
  )

  expect_true(file.exists(result$data_file))
  expect_true(file.exists(result$anchor_file))
  expect_null(result$delete_file)
  expect_true(file.exists(result$control_file))
  expect_true(file.exists(result$bat_file))
  expect_false(file.exists(result$report_file))
  expect_null(result$results)

  ctrl <- readLines(result$control_file)
  expect_true(any(grepl("^NI=3;$", ctrl)))

  expect_null(result$contents$delete)
  expect_null(result$contents$person)
  expect_equal(result$contents$data, readLines(result$data_file))
  expect_equal(result$contents$anchor, readLines(result$anchor_file))
  expect_equal(result$contents$control, readLines(result$control_file))
  expect_equal(result$contents$bat, readLines(result$bat_file))
})

test_that("winsteps_estimate with keep_items writes a delete file", {
  data <- data.frame(
    id = rep("001", 4),
    item = c("A", "B", "C", "D"),
    score = c(1, 0, 1, 0)
  )
  tmp_dir <- tempfile()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  old <- getOption("winstepsR.exe_path")
  options(winstepsR.exe_path = "Winsteps.exe")
  on.exit(options(winstepsR.exe_path = old), add = TRUE)

  result <- winsteps_estimate(
    data = data, id_col = "id", item_col = "item", score_col = "score",
    items = c("A", "B", "C", "D"), anchor_values = c(-1, -0.5, 0.5, 1),
    keep_items = c("A", "B"),
    run_id = "domain_run", working_dir = tmp_dir, run = FALSE
  )

  expect_true(file.exists(result$delete_file))
  deleted <- readLines(result$delete_file)
  expect_length(deleted, 2)
  expect_true(any(grepl("\tC$", deleted)))
  expect_true(any(grepl("\tD$", deleted)))

  expect_equal(result$contents$delete, deleted)
})

test_that("winsteps_write_bat errors clearly when no exe path is configured", {
  old <- getOption("winstepsR.exe_path")
  options(winstepsR.exe_path = NULL)
  on.exit(options(winstepsR.exe_path = old))

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  expect_error(
    winsteps_write_bat(tmp, control_file = "c.ctr", out_file = "o.csv"),
    "winsteps_exe"
  )
})

test_that("winsteps_run refuses to run on non-Windows platforms", {
  skip_on_os("windows")
  expect_error(winsteps_run(tempfile()), "only works on Windows")
})
