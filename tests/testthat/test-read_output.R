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

test_that("NAME is read as character regardless of whether IDs look numeric", {
  # C7: type guessing returned character for "00123" but numeric for "123",
  # so the column's type depended on which persons were in the run.
  with_ids <- function(ids) {
    tmp <- tempfile()
    writeLines(
      c("; header", "ENTRY MEASURE COUNT SCORE NAME",
        paste("1 0.53 40 32", ids[1]),
        paste("2 -1.10 40 18", ids[2])),
      tmp
    )
    on.exit(unlink(tmp))
    winsteps_read_person_output(tmp)$NAME
  }

  expect_identical(with_ids(c("00123", "00456")), c("00123", "00456"))
  expect_identical(with_ids(c("123", "456")), c("123", "456"))
})

test_that("numeric columns are still guessed as numeric", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(
    c("; header", "ENTRY MEASURE COUNT SCORE NAME", "1 0.53 40 32 001"),
    tmp
  )
  out <- winsteps_read_person_output(tmp)
  expect_type(out$MEASURE, "double")
  expect_type(out$NAME, "character")
})

test_that("a PFILE with column names but no data rows keeps the full schema", {
  # R11: the caller should get the same columns on an empty day as on a full
  # one, so binding runs together does not hit a schema mismatch.
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c("; header", "ENTRY MEASURE COUNT SCORE NAME"), tmp)
  out <- winsteps_read_person_output(tmp)
  expect_equal(nrow(out), 0)
  expect_true(all(c("ENTRY", "MEASURE", "COUNT", "SCORE", "NAME") %in% names(out)))
})

test_that("col_types can be overridden by the caller", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(
    c("; header", "ENTRY MEASURE COUNT SCORE NAME", "1 0.53 40 32 001"),
    tmp
  )
  out <- winsteps_read_person_output(
    tmp, col_types = readr::cols(MEASURE = readr::col_character())
  )
  expect_type(out$MEASURE, "character")
})
