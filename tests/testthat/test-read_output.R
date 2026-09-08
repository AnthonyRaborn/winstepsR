test_that("read_person_output returns zero-row tibble when file is missing and empty_ok", {
  out <- winsteps_read_person_output(tempfile(), empty_ok = TRUE)
  expect_equal(nrow(out), 0)
})

test_that("read_person_output errors when file is missing and !empty_ok", {
  expect_error(
    winsteps_read_person_output(tempfile(), empty_ok = FALSE),
    "not found"
  )
})

test_that("read_person_output returns zero-row tibble for header-only file", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines("; PERSON header comment", tmp)
  out <- winsteps_read_person_output(tmp)
  expect_equal(nrow(out), 0)
})

test_that("read_report returns character(0) for a missing file when empty_ok", {
  out <- winsteps_read_report(tempfile(), empty_ok = TRUE)
  expect_equal(out, character(0))
})

test_that("read_report errors when file is missing and !empty_ok", {
  expect_error(winsteps_read_report(tempfile(), empty_ok = FALSE), "not found")
})

test_that("read_report returns raw lines from a report file", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c("TABLE 17.1", "some table content", "more content"), tmp)
  expect_equal(
    winsteps_read_report(tmp),
    c("TABLE 17.1", "some table content", "more content")
  )
})

test_that("read_person_output parses a simple whitespace table", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(
    c(
      "; header",
      "ENTRY MEASURE COUNT SCORE NAME",
      "1 0.53 40 32 001",
      "2 -1.10 40 18 002"
    ),
    tmp
  )
  out <- winsteps_read_person_output(tmp)
  expect_equal(nrow(out), 2)
  expect_true(all(c("MEASURE", "COUNT", "SCORE", "NAME") %in% names(out)))
})
