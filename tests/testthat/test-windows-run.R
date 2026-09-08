# Windows-only tests for the run = TRUE path.
#
# Winsteps is licensed software and cannot be installed on a CI runner, so
# these substitute a stub batch file for Winsteps.exe. That does not test
# anything about Winsteps -- it tests this package's plumbing around it: that
# the generated .bat is runnable by cmd.exe, that it locates its own directory,
# that a non-zero exit status is noticed, and that the output files are read
# back. Every one of those is unreachable on any other platform, because
# winsteps_run() refuses to proceed there.

skip_unless_windows <- function() {
  skip_if(.Platform$OS.type != "windows", "Windows-only: winsteps_run() needs shell()")
}

# A stand-in for Winsteps.exe. `copies` names extdata files to drop into the
# working directory (under the names the control file asked for); `status` is
# the exit code to report.
write_stub_exe <- function(dir, copies = character(0), status = 0L) {
  path <- file.path(dir, "stub_winsteps.bat")
  lines <- "@echo off"
  for (target in names(copies)) {
    src <- normalizePath(
      system.file("extdata", copies[[target]], package = "winstepsR"),
      winslash = "\\", mustWork = TRUE
    )
    lines <- c(lines, sprintf('copy /Y "%s" "%s" >nul', src, target))
  }
  lines <- c(lines, sprintf("exit /b %d", status))
  writeLines(lines, path)
  normalizePath(path, winslash = "\\", mustWork = TRUE)
}

example_inputs <- function() {
  responses <- utils::read.csv(
    system.file("extdata", "example_responses.csv", package = "winstepsR"),
    colClasses = c("character", "character", "integer")
  )
  anchor_tbl <- utils::read.csv(
    system.file("extdata", "example_anchors.csv", package = "winstepsR"),
    colClasses = c("character", "numeric", "character")
  )
  list(responses = responses,
       anchors = winsteps_anchors(anchor_tbl$item, anchor_tbl$value))
}

test_that("winsteps_run executes a generated .bat and reports its status", {
  skip_unless_windows()

  dir <- tempfile()
  dir.create(dir)
  exe <- write_stub_exe(dir, status = 0L)

  bat <- file.path(dir, "run.bat")
  winsteps_write_bat(bat, "control.ctr", "OUT.csv", winsteps_exe = exe)

  before <- getwd()
  expect_equal(winsteps_run(bat), 0L)
  # the batch file cd's to its own directory, so the session's must be intact
  expect_equal(getwd(), before)
})

test_that("a non-zero exit status from Winsteps raises an error", {
  skip_unless_windows()

  dir <- tempfile()
  dir.create(dir)
  exe <- write_stub_exe(dir, status = 3L)
  bat <- file.path(dir, "run.bat")
  winsteps_write_bat(bat, "control.ctr", "OUT.csv", winsteps_exe = exe)

  expect_error(winsteps_run(bat), "exited with status")
  # and the status is still available when the caller opts out of the error
  expect_equal(winsteps_run(bat, error_on_failure = FALSE), 3L)
})

test_that("the .bat runs from any working directory", {
  skip_unless_windows()

  dir <- tempfile()
  dir.create(dir)
  exe <- write_stub_exe(dir, c(person.out = "example_full_person.out"))
  bat <- file.path(dir, "run.bat")
  winsteps_write_bat(bat, "control.ctr", "OUT.csv", winsteps_exe = exe)

  # launched from somewhere else entirely; output must still land beside the bat
  elsewhere <- tempfile()
  dir.create(elsewhere)
  old <- setwd(elsewhere)
  on.exit(setwd(old), add = TRUE)

  winsteps_run(bat)
  expect_true(file.exists(file.path(dir, "person.out")))
  expect_false(file.exists(file.path(elsewhere, "person.out")))
})

test_that("winsteps_estimate reads back everything a successful run wrote", {
  skip_unless_windows()

  inputs <- example_inputs()
  work <- tempfile()
  dir.create(work, recursive = TRUE)
  exe <- write_stub_exe(work, c(
    person.out = "example_full_person.out",
    item.out   = "example_full_item.out",
    OUT.csv    = "example_full_report.csv"
  ))

  result <- winsteps_estimate(
    data = inputs$responses,
    id_col = "person_id", item_col = "item", score_col = "score",
    anchors = inputs$anchors,
    run_id = "stubbed", working_dir = work,
    control_args = list(tfile = "17.1"),
    winsteps_exe = exe, run = TRUE
  )

  expect_equal(nrow(result$results), 200)
  expect_equal(nrow(result$items), 12)
  expect_equal(result$items$NAME, inputs$anchors$items)
  expect_length(result$contents$person, 202)
  expect_true("item" %in% names(result$contents))
  expect_s3_class(result$contents$report, "winsteps_report")

  # the Winsteps-only timing is recorded once a run actually happens
  expect_s3_class(result$winsteps_elapsed, "difftime")
  expect_gte(as.numeric(result$winsteps_elapsed), 0)

  # and summary() reports a real distribution rather than "not run"
  s <- summary(result)
  expect_true(s$ran)
  expect_equal(s$n, 200)
  expect_false(is.null(s$measure))
})

test_that("a run that succeeds but writes no output is an error", {
  skip_unless_windows()

  inputs <- example_inputs()
  work <- tempfile()
  dir.create(work, recursive = TRUE)
  exe <- write_stub_exe(work, status = 0L)   # exits cleanly, writes nothing

  expect_error(
    winsteps_estimate(
      data = inputs$responses,
      id_col = "person_id", item_col = "item", score_col = "score",
      anchors = inputs$anchors,
      run_id = "empty", working_dir = work,
      winsteps_exe = exe, run = TRUE
    ),
    "wrote no person output file"
  )
})

test_that("a missing item file is caught even when the person file arrived", {
  skip_unless_windows()

  inputs <- example_inputs()
  work <- tempfile()
  dir.create(work, recursive = TRUE)
  exe <- write_stub_exe(work, c(person.out = "example_full_person.out"))

  expect_error(
    winsteps_estimate(
      data = inputs$responses,
      id_col = "person_id", item_col = "item", score_col = "score",
      anchors = inputs$anchors,
      run_id = "no_item", working_dir = work,
      winsteps_exe = exe, run = TRUE
    ),
    "wrote no item output file"
  )
})
