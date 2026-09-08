make_result <- function(res) {
  structure(
    list(run_id = "exam1", run_dir = "/tmp/exam1",
         contents = list(anchor = rep("a", 3)), results = res),
    class = "winsteps_result"
  )
}

test_that("summary reports the measure distribution and nothing thresholded", {
  res <- tibble::tibble(
    ENTRY = 1:5,
    MEASURE = c(-2, -1, 0, 1, 2),
    COUNT = rep(4, 5),
    SCORE = c(0, 1, 2, 3, 4),
    MODLSE = c(0.3, 0.4, 0.5, 0.4, 0.3),
    NAME = sprintf("%03d", 1:5)
  )
  s <- summary(make_result(res))

  expect_s3_class(s, "summary.winsteps_result")
  expect_equal(s$n, 5)
  expect_equal(unname(s$measure[["mean"]]), 0)
  expect_equal(unname(s$measure[["median"]]), 0)
  expect_equal(unname(s$measure[["min"]]), -2)
  expect_equal(unname(s$measure[["max"]]), 2)
  expect_equal(unname(s$se[["mean"]]), 0.38)

  # arithmetic boundaries, not chosen cut-offs
  expect_equal(s$n_zero, 1)
  expect_equal(s$n_perfect, 1)

  # nothing fit-related or thresholded is reported
  out <- capture.output(print(s))
  expect_false(any(grepl("misfit|MNSQ|outfit|infit", out, ignore.case = TRUE)))
  expect_true(any(grepl("adds no cut-offs", out)))
})

test_that("summary handles a run that was not performed", {
  s <- summary(make_result(NULL))
  expect_false(s$ran)
  expect_null(s$measure)
  expect_match(capture.output(print(s))[2], "not run")
})

test_that("summary degrades when the PFILE lacks the expected columns", {
  s <- summary(make_result(tibble::tibble(ENTRY = 1:2, NAME = c("a", "b"))))
  expect_equal(s$n, 2)
  expect_null(s$measure)
  expect_null(s$n_zero)
  expect_true(any(grepl("no MEASURE column", capture.output(print(s)))))
})

test_that("summary ignores non-finite measures rather than propagating them", {
  res <- tibble::tibble(MEASURE = c(1, 2, NA, Inf), COUNT = rep(4, 4),
                        SCORE = c(1, 2, 3, 4))
  s <- summary(make_result(res))
  expect_equal(unname(s$measure[["mean"]]), 1.5)
  expect_equal(unname(s$measure[["max"]]), 2)
})

test_that("summary of a report indexes its tables", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c("TABLE 17.1 PERSON MEASURE ORDER", rep("d", 5),
               "TABLE 3.1 SUMMARY", rep("d", 9)), tmp)
  s <- summary(winsteps_read_report(tmp))

  expect_s3_class(s, "data.frame")
  expect_equal(s$table, c("17.1", "3.1"))
  expect_equal(s$start, c(1L, 7L))
  expect_equal(s$lines, c(6L, 10L))
})

test_that("summary of a report with no tables returns zero rows", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c("chatter", "no tables"), tmp)
  s <- summary(winsteps_read_report(tmp))
  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 0)
})
