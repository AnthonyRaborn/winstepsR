test_that("prepare_person_data pivots and pads correctly", {
  data <- data.frame(
    id = c("1", "1", "2", "2"),
    item = c("A", "B", "A", "B"),
    score = c(1, 0, NA, 1),
    stringsAsFactors = FALSE
  )

  prepared <- winsteps_prepare_person_data(
    data, id_col = "id", item_col = "item", score_col = "score",
    id_width = 3, item_order = c("A", "B")
  )

  expect_equal(prepared$items, c("A", "B"))
  expect_equal(prepared$n_items, 2)
  expect_equal(prepared$id_width, 3)
  expect_equal(prepared$item1, 3 + 1 + 1)
  expect_equal(prepared$lines, c("1  *10", "2  *.1"))
})

test_that("prepare_person_data errors on id_width too small", {
  data <- data.frame(id = "1000", item = "A", score = 1)
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score", id_width = 2),
    "id_width"
  )
})

test_that("prepare_person_data errors on unknown item_order entries", {
  data <- data.frame(id = "1", item = "A", score = 1)
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score", item_order = c("A", "B")),
    "item_order"
  )
})

test_that("write_person_data writes lines to file", {
  prepared <- list(lines = c("foo", "bar"))
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_person_data(prepared, tmp)
  expect_equal(readLines(tmp), c("foo", "bar"))
})

test_that("prepare_person_data rejects multi-character response codes", {
  # C2: a score of 10 used to be pasted in whole, silently shifting every
  # response column after it while NI/ITEM1 still described the narrow layout.
  data <- data.frame(
    id = c("1", "1"),
    item = c("A", "B"),
    score = c(10, 1)
  )
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "exactly one character"
  )
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "XWIDE"
  )
})

test_that("prepare_person_data rejects a multi-character missing_code", {
  data <- data.frame(id = "1", item = "A", score = NA_real_)
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score", missing_code = ".."),
    "missing_code must be exactly one character"
  )
})

test_that("prepare_person_data still accepts ordinary single-character codes", {
  data <- data.frame(
    id = c("1", "1", "2", "2"),
    item = c("A", "B", "A", "B"),
    score = c(1, 0, 2, NA)
  )
  prepared <- winsteps_prepare_person_data(
    data, "id", "item", "score", item_order = c("A", "B")
  )
  expect_equal(prepared$lines, c("1*10", "2*2."))
})

test_that("prepare_person_data rejects zero-row input", {
  # C5: an empty cohort used to yield id_width/item1 of -Inf and one
  # delimiter-only line for a person who does not exist.
  data <- data.frame(id = character(0), item = character(0), score = numeric(0))
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "no rows"
  )
})

test_that("prepare_person_data names the duplicated person and item", {
  # R6: this used to fail inside vctrs with a message naming neither.
  data <- data.frame(
    id = c("1", "1", "1"),
    item = c("A", "A", "B"),
    score = c(1, 0, 1)
  )
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "more than one response"
  )
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "1/A"
  )
})

test_that("prepare_person_data rejects a duplicated item_order", {
  data <- data.frame(id = c("1", "1"), item = c("A", "B"), score = c(1, 0))
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score",
                                 item_order = c("A", "B", "A")),
    "items must be unique"
  )
})

test_that("write_person_data rejects anything that is not prepared output", {
  # R10: any list with a `lines` element used to reach writeLines().
  tmp <- tempfile()
  on.exit(unlink(tmp))
  expect_error(winsteps_write_person_data(list(lines = c(1, 2)), tmp),
               "character `lines` element")
  expect_error(winsteps_write_person_data("not a list", tmp),
               "winsteps_prepare_person_data")
})

test_that("unreadable scores warn instead of silently becoming missing", {
  # R5: a "correct"/"incorrect" column used to produce an all-missing data
  # file and a clean run that scored nobody.
  data <- data.frame(
    id = c("1", "1"),
    item = c("A", "B"),
    score = c("correct", "0")
  )
  expect_warning(
    prepared <- winsteps_prepare_person_data(data, "id", "item", "score"),
    "could not be read as numbers"
  )
  expect_equal(prepared$lines, "1*.0")
})

test_that("genuinely missing scores do not warn", {
  data <- data.frame(
    id = c("1", "1"),
    item = c("A", "B"),
    score = c(NA_real_, 0)
  )
  expect_no_warning(
    prepared <- winsteps_prepare_person_data(data, "id", "item", "score")
  )
  expect_equal(prepared$lines, "1*.0")
})
