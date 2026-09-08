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

test_that("winsteps_write_bat quotes for cmd.exe regardless of host platform", {
  # C1: shQuote() defaults to the *writing* host's convention, so a .bat
  # generated off Windows used to be single-quoted and unrunnable by cmd.exe.
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_bat(
    tmp,
    control_file = "control.ctr",
    out_file = "OUT.csv",
    winsteps_exe = "C:/Program Files/Winsteps/Winsteps.exe"
  )
  line <- readLines(tmp)

  expect_match(line, '"C:/Program Files/Winsteps/Winsteps.exe"', fixed = TRUE)
  expect_match(line, '"control.ctr"', fixed = TRUE)
  expect_match(line, '"OUT.csv"', fixed = TRUE)
  # no UNIX-style single quoting anywhere in the line
  expect_false(grepl("'", line, fixed = TRUE))
})

test_that("a non-zero Winsteps exit status raises an error", {
  # C3: the status used to be discarded, so a crashed run was indistinguishable
  # from a cohort with no eligible persons.
  bat <- file.path(tempdir(), "run.bat")
  expect_error(
    winstepsR:::winsteps_stop_on_status(1L, bat),
    "exited with status 1"
  )
  expect_error(
    winstepsR:::winsteps_stop_on_status(255L, bat),
    "should be trusted"
  )
})

test_that("a zero or unavailable exit status is not treated as a failure", {
  bat <- file.path(tempdir(), "run.bat")
  expect_silent(winstepsR:::winsteps_stop_on_status(0L, bat))
  expect_silent(winstepsR:::winsteps_stop_on_status(NA_integer_, bat))
  expect_silent(winstepsR:::winsteps_stop_on_status(NULL, bat))
})

test_that("winsteps_estimate sets NAMLEN from the prepared ID width", {
  data <- data.frame(
    id = rep(c("00001", "00002"), each = 2),
    item = rep(c("A", "B"), 2),
    score = c(1, 0, 1, 1)
  )
  tmp_dir <- tempfile()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  old <- getOption("winstepsR.exe_path")
  options(winstepsR.exe_path = "Winsteps.exe")
  on.exit(options(winstepsR.exe_path = old), add = TRUE)

  result <- winsteps_estimate(
    data = data, id_col = "id", item_col = "item", score_col = "score",
    items = c("A", "B"), anchor_values = c(-0.5, 0.5),
    run_id = "namlen_run", working_dir = tmp_dir, run = FALSE
  )
  expect_true("NAMLEN=5;" %in% result$contents$control)
  expect_true("ITEM1=7;" %in% result$contents$control)
})

test_that("winsteps_estimate rejects a run_id that is not a plain directory name", {
  # R1: run_id was interpolated into a path unchecked, so "../x" escaped
  # working_dir entirely.
  data <- data.frame(id = "1", item = "A", score = 1)
  args <- list(
    data = data, id_col = "id", item_col = "item", score_col = "score",
    items = "A", anchor_values = 0, run = FALSE, working_dir = tempfile()
  )
  for (bad in c("../escaped", "a/b", "..", "")) {
    expect_error(
      do.call(winsteps_estimate, c(args, list(run_id = bad))),
      "run_id must be a single non-empty name"
    )
  }
})

test_that("control_args cannot silently collide with derived arguments", {
  # R3: this used to fail with "formal argument matched by multiple actual
  # arguments", which named neither control_args nor the fix.
  data <- data.frame(id = "1", item = "A", score = 1)
  expect_error(
    winsteps_estimate(
      data = data, id_col = "id", item_col = "item", score_col = "score",
      items = "A", anchor_values = 0, run = FALSE,
      working_dir = tempfile(), control_args = list(n_items = 999)
    ),
    "control_args cannot set argument\\(s\\).*n_items"
  )
})

test_that("control_args still accepts arguments the wrapper does not derive", {
  data <- data.frame(id = "1", item = "A", score = 1)
  result <- winsteps_estimate(
    data = data, id_col = "id", item_col = "item", score_col = "score",
    items = "A", anchor_values = 0, run = FALSE,
    working_dir = tempfile(), winsteps_exe = "Winsteps.exe",
    control_args = list(codes = "012", extra = "XWIDE=1;", tfile = "17.1")
  )
  expect_true("CODES=012;" %in% result$contents$control)
  expect_true("XWIDE=1;" %in% result$contents$control)
  expect_true("TFILE=*" %in% result$contents$control)
})

test_that("run paths live in one place and only include a delete file when needed", {
  paths <- winstepsR:::winsteps_run_paths("/tmp/run1")
  expect_null(paths$delete_file)
  expect_equal(basename(paths$control_file), "control.ctr")
  expect_true(all(dirname(unlist(paths)) == "/tmp/run1"))

  subset_paths <- winstepsR:::winsteps_run_paths("/tmp/run1", subset = TRUE)
  expect_equal(basename(subset_paths$delete_file), "delete.txt")
})

test_that("winsteps_estimate returns a classed result that prints a summary", {
  data <- data.frame(
    id = rep(sprintf("%05d", 1:3), each = 4),
    item = rep(c("q1", "q2", "q3", "q4"), 3),
    score = rep(c(1, 0, 1, 1), 3)
  )
  tmp_dir <- tempfile()
  on.exit(unlink(tmp_dir, recursive = TRUE))

  result <- winsteps_estimate(
    data = data, id_col = "id", item_col = "item", score_col = "score",
    items = c("q1", "q2", "q3", "q4"), anchor_values = c(-1, -0.3, 0.4, 1.2),
    run_id = "exam1", working_dir = tmp_dir, winsteps_exe = "Winsteps.exe",
    run = FALSE
  )
  expect_s3_class(result, "winsteps_result")
  expect_equal(result$run_id, "exam1")
  expect_equal(result$run_dir, file.path(tmp_dir, "exam1"))

  out <- capture.output(print(result))
  expect_match(out[1], "exam1")
  expect_true(any(grepl("4 anchored", out)))
  expect_true(any(grepl("Persons   3", out)))
  expect_true(any(grepl("not run", out)))
  # files Winsteps has not written yet are not claimed as present
  expect_false(any(grepl("person.out", out)))
  # and the whole thing stays a summary
  expect_lt(length(out), 15)
})

test_that("the printed summary reports the item subset when one is used", {
  data <- data.frame(
    id = rep("00001", 4),
    item = c("q1", "q2", "q3", "q4"),
    score = c(1, 0, 1, 1)
  )
  result <- winsteps_estimate(
    data = data, id_col = "id", item_col = "item", score_col = "score",
    items = c("q1", "q2", "q3", "q4"), anchor_values = c(-1, -0.3, 0.4, 1.2),
    keep_items = c("q1", "q2"), run_id = "dom", working_dir = tempfile(),
    winsteps_exe = "Winsteps.exe", run = FALSE
  )
  out <- capture.output(print(result))
  expect_true(any(grepl("2 estimated \\(2 excluded via IDFILE\\)", out)))
  expect_true(any(grepl("delete.txt", out)))
})

test_that("the printed summary reports measures when Winsteps has run", {
  # The run = TRUE branch cannot execute off Windows, so exercise the print
  # method against a result shaped the way that branch produces.
  result <- structure(
    list(
      run_id = "exam1", run_dir = "/tmp/exam1",
      data_file = "/tmp/exam1/data.dat", anchor_file = "/tmp/exam1/anchor.txt",
      delete_file = NULL, control_file = "/tmp/exam1/control.ctr",
      bat_file = "/tmp/exam1/run.bat", person_file = "/tmp/exam1/person.out",
      report_file = "/tmp/exam1/OUT.csv",
      contents = list(
        data = rep("00001*1011", 40), anchor = rep("a", 4), delete = NULL,
        control = "NI=4;", bat = "x", person = rep("p", 41), report = "TABLE 17.1"
      ),
      results = tibble::tibble(NAME = rep("00001", 40), MEASURE = rep(0.5, 40))
    ),
    class = "winsteps_result"
  )
  out <- capture.output(print(result))
  expect_true(any(grepl("run; 40 person measures returned", out)))
  expect_true(any(grepl("\\$results   tibble 40 x 2", out)))
  expect_true(any(grepl("person, report", out)))
})
