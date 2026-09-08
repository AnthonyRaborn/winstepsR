test_that("write_anchor_file writes seq/value/;/item lines", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_anchor_file(c("A", "B"), c(-0.5, 1.2345), tmp, digits = 2)
  lines <- readLines(tmp)
  # format() aligns decimal places across the vector (-0.50, not -0.5);
  # value is unchanged, just padded, which Winsteps parses identically.
  expect_equal(lines, c("1\t-0.50\t;\tA", "2\t1.23\t;\tB"))
})

test_that("write_anchor_file errors on length mismatch", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  expect_error(winsteps_write_anchor_file(c("A", "B"), c(1), tmp), "same length")
})

test_that("write_item_subset_file lists only excluded items with correct sequence numbers", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_item_subset_file(c("A", "B", "C", "D"), keep = c("B", "D"), file = tmp)
  lines <- readLines(tmp)
  expect_equal(lines, c("1\t;\tA", "3\t;\tC"))
})

test_that("write_item_subset_file errors when keep contains unknown items", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  expect_error(
    winsteps_write_item_subset_file(c("A", "B"), keep = "Z", file = tmp),
    "not present in items"
  )
})

test_that("write_anchor_file rejects non-finite anchor values", {
  # C6: NA used to be written literally as the text "NA", leaving the item
  # unanchored without any error.
  tmp <- tempfile()
  on.exit(unlink(tmp))
  expect_error(
    winsteps_write_anchor_file(c("A", "B"), c(-0.5, NA), tmp),
    "must all be finite"
  )
  expect_error(
    winsteps_write_anchor_file(c("A", "B"), c(-0.5, Inf), tmp),
    "must all be finite"
  )
  # the offending item is named
  expect_error(
    winsteps_write_anchor_file(c("A", "B"), c(-0.5, NA), tmp),
    "B"
  )
})

test_that("item vectors must be unique across the anchor and subset writers", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  expect_error(
    winsteps_write_anchor_file(c("A", "B", "A"), c(1, 2, 3), tmp),
    "items must be unique"
  )
  expect_error(
    winsteps_write_item_subset_file(c("A", "B", "A"), keep = "B", file = tmp),
    "items must be unique"
  )
})

test_that("write_item_subset_file rejects an empty keep set", {
  # R7: an empty keep set used to delete every item silently.
  tmp <- tempfile()
  on.exit(unlink(tmp))
  expect_error(
    winsteps_write_item_subset_file(c("A", "B"), keep = character(0), file = tmp),
    "would delete every item"
  )
})
