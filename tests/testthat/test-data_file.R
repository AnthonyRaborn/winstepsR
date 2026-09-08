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
