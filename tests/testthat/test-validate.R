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
