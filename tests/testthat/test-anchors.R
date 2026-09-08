test_that("winsteps_anchors validates the pair once, at construction", {
  a <- winsteps_anchors(c("q1", "q2", "q3"), c(-0.4, 0.1, 0.6))
  expect_s3_class(a, "winsteps_anchors")
  expect_equal(a$items, c("q1", "q2", "q3"))
  expect_equal(a$values, c(-0.4, 0.1, 0.6))

  expect_error(winsteps_anchors(c("q1", "q2"), 1), "same length")
  expect_error(winsteps_anchors(c("q1", "q1"), c(1, 2)), "items must be unique")
  expect_error(winsteps_anchors(c("q1", "q2"), c(1, NA)), "must all be finite")
  expect_error(winsteps_anchors("q1\tq2", 1), "tabs or semicolons")
})

test_that("winsteps_anchors prints a summary of the bank", {
  a <- winsteps_anchors(paste0("q", 1:10), seq(-2, 2, length.out = 10))
  out <- capture.output(print(a))
  expect_match(out[1], "10 items")
  expect_true(any(grepl("-2 to 2", out)))
  expect_true(any(grepl("q1, q2", out)))
  expect_true(any(grepl("\\.\\.\\.", out)))  # truncated item list
})

test_that("both writers accept an anchors object and a bare item vector", {
  a <- winsteps_anchors(c("A", "B", "C"), c(-0.5, 0, 1.2345))

  f1 <- tempfile(); f2 <- tempfile()
  on.exit(unlink(c(f1, f2)))
  winsteps_write_anchor_file(a, file = f1, digits = 2)
  winsteps_write_anchor_file(c("A", "B", "C"), c(-0.5, 0, 1.2345), f2, digits = 2)
  expect_equal(readLines(f1), readLines(f2))

  d1 <- tempfile(); d2 <- tempfile()
  on.exit(unlink(c(d1, d2)), add = TRUE)
  winsteps_write_item_subset_file(a, keep = c("B"), file = d1)
  winsteps_write_item_subset_file(c("A", "B", "C"), keep = c("B"), file = d2)
  expect_equal(readLines(d1), readLines(d2))
  expect_equal(readLines(d1), c("1\t;\tA", "3\t;\tC"))
})

test_that("resolve_anchors refuses ambiguous or incomplete input", {
  a <- winsteps_anchors("q1", 0)
  expect_error(winstepsR:::resolve_anchors(a, "q1", 0), "not both")
  expect_error(winstepsR:::resolve_anchors(NULL, "q1", NULL), "Supply anchors")
  expect_error(winstepsR:::resolve_anchors(NULL, NULL, NULL), "Supply anchors")
  expect_error(winstepsR:::resolve_anchors(list(items = "q1"), NULL, NULL),
               "must be a winsteps_anchors object")
  expect_equal(winstepsR:::resolve_anchors(NULL, "q1", 0), a)
})
