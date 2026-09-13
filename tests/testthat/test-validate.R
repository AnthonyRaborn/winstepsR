test_that("format_examples shows the first few values and marks the rest", {
  expect_equal(winstepsR:::format_examples(c("A", "B")), "A, B")
  expect_equal(winstepsR:::format_examples(letters[1:5]), "a, b, c, d, e")
  expect_equal(winstepsR:::format_examples(letters[1:6]), "a, b, c, d, e, ...")
  expect_equal(winstepsR:::format_examples(c("A", "B"), quote = TRUE), '"A", "B"')
  expect_equal(winstepsR:::format_examples(c(10, 2)), "10, 2")
})

test_that("check_positive_int accepts whole positive scalars only", {
  expect_silent(winstepsR:::check_positive_int(1L, "x"))
  expect_silent(winstepsR:::check_positive_int(50, "x"))
  for (bad in list(0, -1, 1.5, NA_integer_, "3", c(1, 2), NULL)) {
    expect_error(winstepsR:::check_positive_int(bad, "x"),
                 "x must be a single positive whole number")
  }
})

test_that("check_positive_ints validates a batch, naming the offending one", {
  expect_silent(winstepsR:::check_positive_ints(list(a = 1, b = 50L)))
  expect_error(winstepsR:::check_positive_ints(list(a = 1, b = -1)),
               "b must be a single positive whole number")
  expect_error(winstepsR:::check_positive_ints(list(a = 0, b = 1)),
               "a must be a single positive whole number")
})

test_that("is_known_winsteps_keyword recognizes real keywords case-insensitively", {
  expect_true(winstepsR:::is_known_winsteps_keyword("UDECIMALS"))
  expect_true(winstepsR:::is_known_winsteps_keyword("udecimals"))
  expect_true(winstepsR:::is_known_winsteps_keyword("MPROX"))
  expect_false(winstepsR:::is_known_winsteps_keyword("UDECIMALZ"))
  expect_false(winstepsR:::is_known_winsteps_keyword("NOT_A_KEYWORD"))
})

test_that("is_known_winsteps_keyword recognizes an unambiguous abbreviation", {
  # Winsteps accepts a prefix of a keyword as long as it names exactly one,
  # e.g. UDECIM for UDECIMALS or CONV for CONVERGE.
  expect_true(winstepsR:::is_known_winsteps_keyword("UDECIM"))
  expect_true(winstepsR:::is_known_winsteps_keyword("CONV"))
  # "U" is a prefix of several keywords (UDECIMALS, USCALE, ...), so it does
  # not uniquely name one and is not accepted.
  expect_false(winstepsR:::is_known_winsteps_keyword("U"))
})

test_that("is_known_winsteps_keyword recognizes indexed keyword families", {
  expect_true(winstepsR:::is_known_winsteps_keyword("KEY1"))
  expect_true(winstepsR:::is_known_winsteps_keyword("KEY23"))
  expect_true(winstepsR:::is_known_winsteps_keyword("IVALUE1"))
  expect_false(winstepsR:::is_known_winsteps_keyword("KEY"))
})

test_that("winsteps_keyword_suggestion finds a close known keyword or nothing", {
  expect_equal(winstepsR:::winsteps_keyword_suggestion("UDECIMALZ"), "UDECIMALS")
  expect_null(winstepsR:::winsteps_keyword_suggestion("ZZZZZZZZZZ"))
})

test_that("check_known_keywords warns once per unrecognized keyword", {
  expect_silent(winstepsR:::check_known_keywords(c("MPROX", "LCONV")))
  expect_silent(winstepsR:::check_known_keywords("UDECIM"))
  expect_warning(winstepsR:::check_known_keywords("UDECIMALZ"), "UDECIMALS")
  w <- testthat::capture_warnings(
    winstepsR:::check_known_keywords(c("UDECIMALZ", "UDECIMALZ"))
  )
  expect_length(w, 1)
})

test_that("check_items rejects empty, duplicated and delimiter-bearing names", {
  expect_silent(winstepsR:::check_items(c("A", "B")))
  expect_error(winstepsR:::check_items(character(0)), "items is empty")
  expect_error(winstepsR:::check_items(c("A", "A")), "items must be unique")
  expect_error(winstepsR:::check_items(c("A\tB")), "tabs or semicolons")
  expect_error(winstepsR:::check_items(c("A;B")), "tabs or semicolons")
})

test_that("check_run_id rejects anything that is not a plain directory name", {
  expect_silent(winstepsR:::check_run_id("exam1"))
  for (bad in list("a/b", "a\\b", "..", ".", "", NA_character_, c("a", "b"))) {
    expect_error(winstepsR:::check_run_id(bad), "run_id must be a single non-empty name")
  }
})
