test_that("a real control file round-trips byte for byte", {
  f <- system.file("extdata", "example_full_control.ctr", package = "winstepsR")
  skip_if(f == "", "example control file not installed")

  args <- winsteps_read_control_file(f)
  expect_s3_class(args, "winsteps_control")

  out <- tempfile()
  on.exit(unlink(out))
  do.call(winsteps_write_control_file, c(list(file = out), args))
  expect_equal(readLines(out), readLines(f))
})

test_that("the parsed arguments are the ones that wrote the file", {
  f <- system.file("extdata", "example_full_control.ctr", package = "winstepsR")
  skip_if(f == "", "example control file not installed")
  args <- winsteps_read_control_file(f)

  expect_equal(args$data_file, "data.dat")     # quotes stripped
  expect_equal(args$n_items, 12)               # numeric, not "12"
  expect_equal(args$item1, 6)
  expect_equal(args$namlen, 4)
  expect_equal(args$codes, "01")               # still text, zero intact
  expect_equal(args$iafile, "anchor.txt")
  expect_equal(args$ifile, "item.out")
  expect_equal(args$tfile, "17.1")
  expect_equal(args$item_labels, sprintf("q%02d", 1:12))
  expect_equal(args$estimation$MPROX, 20)
  expect_equal(args$estimation$CONVERGE, "L")
  expect_equal(args$estimation$LCONV, 0.0001)
  # ITEM= is written by the writer itself and must not come back as well
  expect_null(args$estimation$ITEM)
})

test_that("values keep their case and their leading zeros", {
  # Rwinsteps' read.wcmd() lowercases every value, which corrupts paths on a
  # case-sensitive filesystem and any TITLE= text.
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c(
    'TITLE="Spring Form B — Domain 2";',
    'DATA="/Data/Spring/FormB.dat";',
    "CODES=01;",
    "IAFILE=Anchors_FormB.txt;",
    "NI=4;", "ITEM1=6;", "&END"
  ), tmp)
  args <- winsteps_read_control_file(tmp)

  expect_equal(args$estimation$TITLE, "Spring Form B — Domain 2")
  expect_equal(args$data_file, "/Data/Spring/FormB.dat")
  expect_equal(args$iafile, "Anchors_FormB.txt")
  expect_identical(args$codes, "01")
  expect_false(identical(args$codes, 1))
})

test_that("comments are stripped and blank lines ignored", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c(
    "; a leading comment line",
    "",
    "NI=4 ; four items",
    "ITEM1=6;",
    "CODES=01;   ; response codes",
    "&END"
  ), tmp)
  args <- winsteps_read_control_file(tmp)

  expect_equal(args$n_items, 4)
  expect_equal(args$item1, 6)
  expect_equal(args$codes, "01")
  expect_null(args$estimation)
})

test_that("TFILE blocks are collected and other blocks are refused", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c("NI=4;", "ITEM1=6;", "TFILE=*", "17.1", "3.1", "*;", "&END"), tmp)
  expect_equal(winsteps_read_control_file(tmp)$tfile, c("17.1", "3.1"))

  writeLines(c("NI=4;", "IAFILE=*", "1 -0.5", "2 0.5", "*;", "&END"), tmp)
  expect_error(winsteps_read_control_file(tmp), "IAFILE=\\* is an inline block")

  writeLines(c("NI=4;", "TFILE=*", "17.1"), tmp)
  expect_error(winsteps_read_control_file(tmp), "Unterminated TFILE")
})

test_that("a non-default ITEM= warns rather than vanishing", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c("NI=4;", "ITEM1=6;", "ITEM=Question;", "&END"), tmp)
  expect_warning(
    args <- winsteps_read_control_file(tmp),
    "ITEM=Question was not kept"
  )
  expect_null(args$estimation$ITEM)
})

test_that("files without &END or without labels still parse", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  writeLines(c("NI=4;", "ITEM1=6;"), tmp)          # no &END at all
  args <- winsteps_read_control_file(tmp)
  expect_equal(args$n_items, 4)
  expect_null(args$item_labels)

  writeLines(c("NI=4;", "ITEM1=6;", "&END"), tmp)  # &END, no labels
  expect_null(winsteps_read_control_file(tmp)$item_labels)

  # labels without a closing END LABELS
  writeLines(c("NI=2;", "ITEM1=6;", "&END", "a", "b"), tmp)
  expect_equal(winsteps_read_control_file(tmp)$item_labels, c("a", "b"))
})

test_that("a non-numeric layout position is reported, not silently coerced", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c("NI=twelve;", "ITEM1=6;", "&END"), tmp)
  expect_error(winsteps_read_control_file(tmp), "NI= is not a number")
})

test_that("a missing control file errors clearly", {
  expect_error(winsteps_read_control_file(tempfile()), "control file not found")
})

test_that("printing a parsed control file avoids scientific notation", {
  f <- system.file("extdata", "example_full_control.ctr", package = "winstepsR")
  skip_if(f == "", "example control file not installed")
  out <- capture.output(print(winsteps_read_control_file(f)))

  expect_match(out[1], "winsteps_control")
  expect_true(any(grepl("LCONV=0.0001", out, fixed = TRUE)))
  expect_false(any(grepl("1e-04", out, fixed = TRUE)))
})

test_that("ITEM1 is not mistaken for ITEM", {
  # R's `$` partial-matches on lists, so entries$ITEM returned ITEM1's value
  # and every control file setting ITEM1 warned about an ITEM it did not have.
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c("NI=4;", "ITEM1=6;", "&END"), tmp)

  expect_no_warning(args <- winsteps_read_control_file(tmp))
  expect_equal(args$item1, 6)
})
