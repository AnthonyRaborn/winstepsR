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

test_that("read_report returns an empty report for a missing file when empty_ok", {
  out <- winsteps_read_report(tempfile(), empty_ok = TRUE)
  expect_s3_class(out, "winsteps_report")
  expect_length(out, 0)
  expect_equal(as.character(out), character(0))
})

test_that("read_report errors when file is missing and !empty_ok", {
  expect_error(winsteps_read_report(tempfile(), empty_ok = FALSE), "not found")
})

test_that("read_report returns raw lines from a report file", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  lines <- c("TABLE 17.1", "some table content", "more content")
  writeLines(lines, tmp)
  out <- winsteps_read_report(tmp)

  expect_s3_class(out, "winsteps_report")
  expect_equal(as.character(out), lines)
})

test_that("a winsteps_report behaves as a character vector", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  lines <- c("TABLE 17.1", "some table content", "more content")
  writeLines(lines, tmp)
  out <- winsteps_read_report(tmp)

  expect_true(is.character(out))
  expect_length(out, 3)
  expect_equal(out[2], "some table content")
  expect_true(any(grepl("table content", out)))
  # round-trips through writeLines like any other character vector
  tmp2 <- tempfile()
  on.exit(unlink(tmp2), add = TRUE)
  writeLines(out, tmp2)
  expect_equal(readLines(tmp2), lines)
})

test_that("printing a report summarises its tables instead of echoing them", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(
    c("TABLE 17.1 PERSON MEASURE ORDER", rep("detail", 5),
      "TABLE 3.1 SUMMARY OF MEASURED PERSONS", rep("detail", 9)),
    tmp
  )
  out <- capture.output(print(winsteps_read_report(tmp)))

  expect_match(out[1], "16 lines from 2 tables")
  expect_true(any(grepl("TABLE 17\\.1", out)))
  expect_true(any(grepl("TABLE 3\\.1", out)))
  # the 16-line report does not print 16 lines
  expect_lt(length(out), 10)
})

test_that("printing copes with an empty report and one with no table headings", {
  empty <- capture.output(print(winsteps_read_report(tempfile())))
  expect_match(empty[1], "0 lines")
  expect_true(any(grepl("no report file", empty)))

  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c("some output", "without headings"), tmp)
  plain <- capture.output(print(winsteps_read_report(tmp)))
  expect_true(any(grepl("no TABLE headings found", plain)))
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

test_that("the comment marker is stripped from the first column name", {
  # Winsteps comments out its column-name line, so the leading ";" runs into
  # the first name. Column set below is the real one from a Winsteps run.
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(
    c("; PERSON FILE",
      paste(";ENTRY MEASURE ST COUNT SCORE MODLSE IN.MSQ INZSTD OUTMSQ OUTZST",
            "DISPL PTMA WEIGHT OBSMA EXPMA PMA-E RMSR WMLE INDF OUTDF NAME"),
      paste("1 0.72 1 3 2 1.25 0.73 -0.54 0.68 -0.56 0 0.87 1 66.7 66.6",
            "0.5 0.4 0.7 1 1 001"),
      paste("2 -0.72 1 3 1 1.25 1.09 0.35 1.05 0.27 0 0.00 1 66.7 66.6",
            "0.5 0.4 0.7 1 1 002")),
    tmp
  )
  out <- winsteps_read_person_output(tmp)

  expect_equal(nrow(out), 2)
  expect_equal(names(out)[1], "ENTRY")
  expect_false(any(grepl("^;", names(out))))
  expect_equal(out$ENTRY, c(1, 2))
  # NAME is still pinned to character despite sitting last in a wide table
  expect_type(out$NAME, "character")
  expect_equal(out$NAME, c("001", "002"))
  expect_equal(ncol(out), 21)
})

test_that("a real Winsteps PFILE parses correctly", {
  # A genuine PFILE from a Windows run (see inst/extdata/README.md). Its
  # measures are not a valid estimation -- it came from the W2 probe, which
  # deliberately fed Winsteps a malformed data file -- but its *layout* is
  # exactly what Winsteps writes, which is what this test is for.
  f <- system.file("extdata", "pfile_example.out", package = "winstepsR")
  skip_if(f == "", "example PFILE not installed")
  out <- winsteps_read_person_output(f)

  expect_equal(nrow(out), 2)
  expect_equal(ncol(out), 21)

  # the comment marker Winsteps puts on its column-name line is stripped
  expect_equal(names(out)[1], "ENTRY")
  expect_false(any(grepl("^;", names(out))))
  expect_equal(
    names(out),
    c("ENTRY", "MEASURE", "ST", "COUNT", "SCORE", "MODLSE", "IN.MSQ", "INZSTD",
      "OUTMSQ", "OUTZST", "DISPL", "PTMA", "WEIGHT", "OBSMA", "EXPMA", "PMA-E",
      "RMSR", "WMLE", "INDF", "OUTDF", "NAME")
  )

  # Winsteps writes values without a leading zero (".72", "-.72"); they must
  # still arrive as numbers, not text
  expect_type(out$MEASURE, "double")
  expect_equal(out$MEASURE, c(0.72, -0.72))
  expect_equal(out$PTMA, c(0.87, 0))

  # person IDs keep their leading zeros and stay character
  expect_type(out$NAME, "character")
  expect_equal(out$NAME, c("001", "002"))

  # the trailing blank line does not become a row
  expect_equal(out$ENTRY, c(1, 2))
})

test_that("an existing but empty report file reads as an empty report", {
  # Winsteps writes a zero-byte batch report when no TFILE= is requested.
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp))

  out <- winsteps_read_report(tmp)
  expect_s3_class(out, "winsteps_report")
  expect_length(out, 0)
  expect_match(capture.output(print(out))[1], "0 lines")
})

test_that("read_item_output parses the same layout as the person file", {
  # IFILE and PFILE share a format. No real IFILE fixture exists yet, so this
  # is built from the layout the real PFILE confirmed.
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(
    c("; ITEM  C:\\\\temp\\\\control.ctr  Sep 08 2026 11:19",
      ";ENTRY MEASURE ST COUNT SCORE MODLSE IN.MSQ DISPL NAME",
      "     1    -.50  1   2.0   1.0   1.25    .73   .00 A",
      "     2     .00  1   2.0   1.0   1.25   1.09  -.02 B",
      "     3     .50  1   2.0   2.0   1.25    .88   .01 C"),
    tmp
  )
  out <- winsteps_read_item_output(tmp)

  expect_equal(nrow(out), 3)
  expect_equal(names(out)[1], "ENTRY")
  expect_equal(out$MEASURE, c(-0.5, 0, 0.5))
  # DISPL is the anchor-verification column
  expect_equal(out$DISPL, c(0, -0.02, 0.01))
  # item names stay character, for the same reason person IDs do
  expect_type(out$NAME, "character")
  expect_equal(out$NAME, c("A", "B", "C"))
})

test_that("read_item_output handles missing and empty files like the person reader", {
  expect_equal(nrow(winsteps_read_item_output(tempfile())), 0)
  expect_error(
    winsteps_read_item_output(tempfile(), empty_ok = FALSE),
    "item output file not found"
  )

  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines("; ITEM header only", tmp)
  expect_equal(nrow(winsteps_read_item_output(tmp)), 0)
  expect_error(winsteps_read_item_output(tmp, empty_ok = FALSE), "no data rows")
})

test_that("numeric-looking item names stay character", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(
    c("; ITEM", ";ENTRY MEASURE NAME", "1 -.5 101", "2 .5 102"),
    tmp
  )
  expect_type(winsteps_read_item_output(tmp)$NAME, "character")
  expect_equal(winsteps_read_item_output(tmp)$NAME, c("101", "102"))
})
